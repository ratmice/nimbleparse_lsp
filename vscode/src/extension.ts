import * as vscode from 'vscode';
import * as path from 'path';
import { Uri } from 'vscode';

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
    // This might be needed eventually if we distribute the nimbleparse_lsp binary
    // inside the package or download it from the package,
    //
    // Currently we rely on this having been installed into PATH as part of the build process,
    // const lsp_path_relative = path.resolve(__dirname, "nimbleparse_lsp");

    const lsp_path = "nimbleparse_lsp";

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
                let path = uri.path;
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


    const state_table_command = 'nimbleparse_lsp.statetable';
    context.subscriptions.push(
        vscode.commands.registerCommand(state_table_command, async () => {
            let uri: Uri | undefined = vscode.window.activeTextEditor?.document.uri;
            if (uri) {
                const params: ExecuteCommandParams = { command: state_table_command, arguments:[uri.toString()]};
                // The following uses two requests, first to generate the state table,
                // Then to request the resulting state table
                // The reason for this is I suppose it makes it easier to update the statetable on
                // parser did_change events and push that to the editor eventually, or as an option.
                //
                // If that is done or optionally done we can drop the first `sendCommand` bit, since it
                // will be part of the parse loop. and just run the cmd_scheme request
                sendCommand(lspClient, params).then((response) => {
                    //let cmd_scheme_uri = vscode.Uri.parse("nimbleparse_lsp://statetable.cmd".concat((uri as Uri).path));
                    let cmd_scheme_uri = Uri.from({scheme: "nimbleparse_lsp", authority: "statetable.cmd", path: uri?.fsPath})
                    console.log(cmd_scheme_uri.toString())
                    cmd_scheme_provider.eventEmitter.fire(cmd_scheme_uri);
                    vscode.workspace.openTextDocument(cmd_scheme_uri).then((textdoc) => {
                        vscode.window.showTextDocument(textdoc, vscode.ViewColumn.Two, true);
                    })
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
