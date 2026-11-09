# PSX for VS Code (local installation)

Highlights `.psx` and `.psxt` and connects them to the PSX stdio language server.
The VSIX contains the client and both grammars; the server is built separately.
Node 22 is required for the server. The extension requires VS Code 1.85 or newer
and the `nwolverson.language-purescript` extension for PureScript highlighting
(tested with 0.2.10). Built-in CSS provides scoped stylesheet highlighting.
Install that grammar extension first if working offline. The server is optional:
highlighting works with `psx.server.enabled` set to `false`.

## Build and install from a fresh checkout

From the PSX repository root:

```sh
npm ci
npx spago build
npm run build -w psx-vscode
npm run package -w psx-vscode
code --install-extension nwolverson.language-purescript
code --install-extension editors/vscode/psx-vscode.vsix --force
```

Alternatively use **Extensions: Install from VSIX…** and select that file. Reload
VS Code, open a consumer workspace folder, and open a `.psx` or `.psxt` file.
No marketplace publication or global PSX installation is needed. The copied
TextMate files come from `editors/textmate`; edit their originals and rebuild.
See that directory's README for scope contracts and highlighting limitations.

To use the Extension Development Host, first run the build above, open
`editors/vscode` as the VS Code folder, select **PSX Extension Development Host**
and press F5. In the new window open the consumer folder and configure it below.
Rebuild and reload that host after editing the extension.

## Separate consumer workspace

Put these settings in the consumer's `.vscode/settings.json`, replacing paths:

```json
{
  "psx.server.command": "node",
  "psx.server.args": ["/absolute/path to/psx/packages/psx-language-server/bin/psx-language-server.mjs", "--stdio"],
  "psx.projectRoot": "/absolute/path to/consumer",
  "psx.purescript": {
    "pursExe": "/absolute/path to/consumer/node_modules/.bin/purs",
    "outputDirectory": "output"
  }
}
```

The command is an executable, never a shell expression. Arguments are an array;
do not add shell quotes around paths with spaces. Absolute paths are recommended;
`${workspaceFolder}` and `~` are not expanded. An empty projectRoot uses the
workspace folder; the server discovers the nearest ancestor with `spago.yaml`.
The launch working directory is that configured root. Optionally set
`psx.purescriptLanguageServer` to an alternative child server executable.
`psx.purescript` forwards compiler settings to that child. Server stdout must
contain only the LSP stream; logging belongs on stderr.

For a multi-root workspace set these values in **each folder's** settings. A
separate PSX process owns each folder's documents and compiler child. Configuration
changes restart the processes; **PSX: Restart Language Servers** does so manually.
Removing folders and closing the extension shuts down their clients. Standalone
files outside workspace folders receive highlighting only. Remote workspaces run
the client/server on the workspace host. Untrusted and virtual workspaces do not
launch processes.

Build consumer sources using the checkout's CLI and the consumer's dependencies:

```sh
node '/absolute/path to/psx/packages/psx-cli/bin/psx.mjs' build src
npx spago build
```

See `packages/psx-language-server/README.md` for backend dependencies, compiler
triggers and virtual-buffer behavior. Parse diagnostics, markup completion and
formatting work without a compiler child; PureScript hover, completion, definition
and type diagnostics require matching compiler output and installed dependencies.

## Process ownership and troubleshooting

PSX owns the language server and its PureScript child **only for PSX/PSXT**.
The PureScript grammar dependency supplies highlighting, not compiler processes.
If you also install `nwolverson.ide-purescript`, keep its document selectors on
ordinary `.purs`; do not associate PSX files with the `purescript` language ID.
Disable its build-on-save and watcher builds in workspaces where those would
race your PSX CLI/build tasks. The PSX child disables full-build-on-save and uses
private virtual buffers; do not run a second PSX client for the same folder.

Use **PSX: Show Output** for stderr and client startup failures. A missing command
prompts for settings; an invalid executable reports the affected folder. Verify
`node`, absolute server path, completed `spago build`, and the consumer root.
After fixing configuration use the restart command. If compiler features alone
are absent, inspect the log, check `pursExe`, build the consumer with that same
compiler, and verify backend imports. Highlighting does not require the server;
missing PureScript colors usually mean the grammar dependency is absent.

## Validation

```sh
npm run build -w psx-vscode
xvfb-run -a npm test -w psx-vscode  # Linux; no xvfb-run on macOS/Windows
npm run package -w psx-vscode
npm run test:textmate
PSX_LSP_REAL=1 npm run test:language-server  # after spago build
xvfb-run -a env PSX_VSCODE_REAL=1 npm test -w psx-vscode
```

The pinned VS Code test host opens both forms in two folders whose paths contain
spaces. A stdio fixture records initialization settings, document forwarding,
restart and graceful shutdown. Its offline grammar dependency uses the pinned,
licensed upstream PureScript grammar from the TextMate fixtures, packaged only
for the test host. The real-server host additionally exercises all five language
features through VS Code provider commands. Set `VSCODE_EXECUTABLE_PATH` to use an existing
VS Code executable instead of downloading the pinned test host. The shared real
server smoke gate checks source diagnostics, completion, hover, definition and
formatting in both forms. For a manual installation check, follow the VSIX recipe,
open both forms in a consumer folder, confirm colors before configuring a server,
then test those features and restart/output commands with the configuration above.
