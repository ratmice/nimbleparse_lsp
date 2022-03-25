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

        if self.server || default {
            cmd!(sh, "cargo install --path server").run()?;
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
    }
}
