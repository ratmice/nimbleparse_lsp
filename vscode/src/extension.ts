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

export function activate(context: vscode.ExtensionContext) {
    // This might be needed eventually if we distribute the nimbleparse_lsp binary
    // inside the package or download it from the package,
    //
    // Currently we rely on this having been installed into PATH as part of the build process,
    // const lsp_path_relative = path.resolve(__dirname, "nimbleparse_lsp");

    const lsp_path = "nimbleparse_lsp";
    
    var tomls = vscode.workspace.workspaceFolders?.map(
        (folder) =>  <ParserWorkspace>({
           workspace: JSON.parse(execFileSync(lsp_path, ["--workspace", Uri.joinPath(folder.uri, "nimbleparse.toml").fsPath]).toString()),
           folder: folder.uri.fsPath
        })
    );
    var docSelector = tomls?.flatMap(
        (toml) => toml.workspace.parsers.map(
            (parser: Parser) => ({
                 // assumes parser.extension includes leading '.'
                 pattern: "**/*" + parser.extension
            })
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

    let clientOptions: LanguageClientOptions = {
        documentSelector: docSelector.concat([{pattern: "**/nimbleparse.toml"}]),
        outputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Info
    };

    lspClient = new LanguageClient("nimbleparse_lsp", serverOptions, clientOptions);
    lspClient.trace = Trace.Verbose;
    context.subscriptions.push(lspClient.start());
}

export async function deactivate(): Promise<void> {   
   if (lspClient) {
     lspClient.stop();
   }
}
