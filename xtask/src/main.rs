mod flags;

use std::env;
use std::path::{Path, PathBuf};
use xshell::{cmd, Shell};
fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

impl flags::Install {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let default = !(self.server || self.client);
        let debug_args = if self.debug { Some("--debug") } else { None };

        if self.server || default {
            let server_args = vec!["--path", "server"];
            if self.console {
                let console_args = vec!["--features", "console"];
                let rustc_flags = std::env::var("RUSTCFLAGS");
                let _pushenv_guard = if let Ok(mut rustc_flags) = rustc_flags {
                    rustc_flags.push_str("--cfg tokio_unstable");
                    sh.push_env("RUSTFLAGS", rustc_flags)
                } else {
                    sh.push_env("RUSTFLAGS", "--cfg tokio_unstable")
                };
                cmd!(
                    sh,
                    "cargo install {debug_args...} {server_args...} {console_args...}"
                )
                .run()?;
            } else {
                cmd!(sh, "cargo install {debug_args...} {server_args...}").run()?;
            }
        }

        if self.client || default {
            let _dir = sh.push_dir("./vscode");
            cmd!(sh, "npm ci").run()?;
            cmd!(sh, "npm run package --scripts-prepend-node-path").run()?;
            cmd!(sh, "code --install-extension nimbleparse_lsp.vsix --force").run()?;
        }

        Ok(())
    }
}

impl flags::Clean {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        {
            let _dir = sh.push_dir("./vscode");
            cmd!(sh, "npm run clean").run()?;
        }
        cmd!(sh, "cargo clean").run()?;
        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    let sh = &Shell::new()?;
    sh.change_dir(project_root());
    let flags = flags::Xtask::from_env()?;

    match flags.subcommand {
        flags::XtaskCmd::Help(_) => {
            println!("{}", flags::Xtask::HELP);
            Ok(())
        }
        flags::XtaskCmd::Install(cmd) => cmd.run(sh),
        flags::XtaskCmd::Clean(cmd) => cmd.run(sh),
    }
}
