import * as vscode from 'vscode';
import * as path from 'path';
import { Uri } from 'vscode';
import * as fs from 'fs';
import {
    Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
    Trace,
    RevealOutputChannelOn,
    ExecuteCommandParams,
    ExecuteCommandRequest,
    RequestType,
} from 'vscode-languageclient/node';

import { config } from 'process';
import { execFileSync } from 'child_process';

let lspClient: LanguageClient;

interface Parser { l_file: string, y_file: string, extension: string }
interface TomlWorkspace {
    parsers: Parser[],
    tests: {dir: string, pass: boolean}
};


interface ParserWorkspace {
    workspace: TomlWorkspace,
    folder: string        
};

namespace ServerDocumentRequest {
    export const type = new RequestType<ServerDocumentParams, string, void>('nimbleparse_lsp/get_server_document');
}

interface ServerDocumentParams {
   // A command like `statetable`
   cmd: string,
   // A Uri from which the command is derived like foo/bar.y
   path: string,
}

export function activate(context: vscode.ExtensionContext) {
    const lsp_path_relative = path.resolve(path.join(context.extensionPath, "bin"), "nimbleparse_lsp");
    const lsp_path_relative_exe = path.resolve(path.join(context.extensionPath, "bin"), "nimbleparse_lsp.exe");
    // Try and find it relative to the extension, or fall back to the PATH.
    const lsp_path = fs.existsSync(lsp_path_relative) ? lsp_path_relative : (fs.existsSync(lsp_path_relative_exe) ? lsp_path_relative_exe : "nimbleparse_lsp");

    // This doesn't quite work if we have a project `foo/`
    // and a `foo/bar/nimbleparse.toml`.
    // As there is no way to ask vscode for all the `workspaceContains:**/nimbleparse.toml`
    // recursive glob patterns that caused the activation
    // https://github.com/microsoft/vscode/issues/44711
    // Woe is me if I have to walk the filesystem again
    // looking for them, especially since: vscode can cause activation because walking the filesystem took too long.
    // and timed out, starting another recursive walk in that case might be well intensioned,
    // but i'm unconvinced it would be a good idea, since it would seem to more often than not take
    // It would merely take a long time looking for something that isn't there and then exit or timeout again.
    //
    // If they ever fix 44711 fix this to use the workspaceContains path that matched.
    var tomls = vscode.workspace.workspaceFolders?.map(
        (folder) =>  <ParserWorkspace>({
           workspace: JSON.parse(execFileSync(lsp_path, ["--workspace", Uri.joinPath(folder.uri, "nimbleparse.toml").fsPath]).toString()),
           folder: folder.uri.fsPath
        })
    );

    if (tomls == undefined || tomls.length == 0) {
	    return;
    }

    var dynSelector = tomls?.flatMap(
        (toml) => toml.workspace.parsers.flatMap(
            (parser: Parser) => ([{
                    // assumes parser.extension includes leading '.'
                    pattern: "**/*" + parser.extension
            }, {
                    pattern: toml.folder + '/' + parser.l_file
            }, {
                    pattern: toml.folder + '/' + parser.y_file
            }])
        )
    );

    const outputChannel = vscode.window.createOutputChannel("nimbleparse_lsp");
    const lsp_exec: Executable = {
        command: lsp_path,
        args: ["--server"],
        options: {
          env: {
            ...process.env,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            RUST_LOG: "info",
          },
        },
      };
    let serverOptions: ServerOptions = {
        run: lsp_exec,
        debug: lsp_exec
    };
    var docSelector = [{pattern: "**/nimbleparse.toml"}];
    if (!(dynSelector == null)) {
        docSelector = docSelector.concat(dynSelector);
    }

    let clientOptions: LanguageClientOptions = {
        documentSelector: docSelector,
        outputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Info
    };

    lspClient = new LanguageClient("nimbleparse_lsp", serverOptions, clientOptions);
    lspClient.trace = Trace.Verbose;
    context.subscriptions.push(lspClient.start());

    const cmd_scheme_provider = new class implements vscode.TextDocumentContentProvider {
        readonly eventEmitter = new vscode.EventEmitter<vscode.Uri>();
        provideTextDocumentContent(uri: vscode.Uri, token: vscode.CancellationToken): vscode.ProviderResult<string> {
            if (uri.scheme == "nimbleparse_lsp") {
                // Turn nimbleparse_lsp_cmd://command/path into a ServerDocumentRequest
                let cmd = uri.authority;
                let path = uri.query;
                let params: ServerDocumentParams = { cmd: cmd, path: path};

                return lspClient.sendRequest(ServerDocumentRequest.type, params).then((response) => {
                    return response
                });
            }
        }
        get onDidChange(): vscode.Event<vscode.Uri> {
            return this.eventEmitter.event;
        }
    }
    context.subscriptions.push(
        vscode.workspace.registerTextDocumentContentProvider('nimbleparse_lsp', cmd_scheme_provider)
    );

    showServerDocumentCommand(context, 'stategraph_core_states', cmd_scheme_provider);
    showServerDocumentCommand(context, 'stategraph_closed_states', cmd_scheme_provider);
    showServerDocumentCommand(context, 'stategraph_core_edges', cmd_scheme_provider);
    showServerDocumentCommand(context, 'stategraph_all_edges', cmd_scheme_provider);
    showServerDocumentCommand(context, 'generictree', cmd_scheme_provider);
    svgDocumentCommand(context, 'railroad.svg', cmd_scheme_provider);
}

function svgDocumentCommand(context: vscode.ExtensionContext, command: string, provider: vscode.TextDocumentContentProvider) {
    const vscode_command = 'nimbleparse_lsp.'.concat(command);
    context.subscriptions.push(
        vscode.commands.registerCommand(vscode_command, async () => {
            let uri: Uri | undefined = vscode.window.activeTextEditor?.document.uri;
            if (uri) {
                let cmd_scheme_uri = Uri.from({scheme: "nimbleparse_lsp", authority: command.concat('.cmd'), path: '/' + command, query: uri?.fsPath});
                provider.eventEmitter.fire(cmd_scheme_uri);
                let foo = vscode.window.createWebviewPanel(command, command, vscode.ViewColumn.Beside);
                vscode.workspace.openTextDocument(cmd_scheme_uri).then((textdoc) => {
                    foo.webview.html = textdoc.getText();
                    foo.reveal(vscode.ViewColumn.Beside, true);
                });
            };
        })
    );
}

function showServerDocumentCommand(context: vscode.ExtensionContext, command: string, provider: vscode.TextDocumentContentProvider) {
    const vscode_command = 'nimbleparse_lsp.'.concat(command);
    context.subscriptions.push(
        vscode.commands.registerCommand(vscode_command, async () => {
            let uri: Uri | undefined = vscode.window.activeTextEditor?.document.uri;
            if (uri) {
                // Send the path in the query string, then use the command name as the path.
                // This is to give the document a name other the source filename from which the command derives.
                // But also to keep hover from spewing errors 'expected relative URL without a base'.
                let cmd_scheme_uri = Uri.from({scheme: "nimbleparse_lsp", authority: command.concat('.cmd'), path: '/' + command, query: uri?.fsPath})
                provider.eventEmitter.fire(cmd_scheme_uri);
                vscode.workspace.openTextDocument(cmd_scheme_uri).then((textdoc) => {
                    vscode.window.showTextDocument(textdoc, {preview: true, viewColumn: vscode.ViewColumn.Beside, preserveFocus: true});
                });
            };
        })
    );
}

function sendCommand(client: LanguageClient, params: ExecuteCommandParams): Promise<any> {
    return client.sendRequest(ExecuteCommandRequest.type, params);
}

// Use this for commands that can spawn from activation events
// TODO waits forever during startup failure probably.
function sendActivationCommand(client: LanguageClient, params: ExecuteCommandParams): Promise<any> {
    return client.onReady().then(() => { sendCommand(client, params) });
}

export async function deactivate(): Promise<void> {   
   if (lspClient) {
     lspClient.stop();
   }
}
