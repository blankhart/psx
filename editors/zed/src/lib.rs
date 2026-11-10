#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn configured_node_launch_preserves_arguments_and_environment() {
        let fixture: serde_json::Value =
            serde_json::from_str(include_str!("../test/fixtures/settings.json")).unwrap();
        let mut settings: LspSettings =
            serde_json::from_value(fixture["lsp"][SERVER].clone()).unwrap();
        settings.binary.as_mut().unwrap().env = Some(std::collections::HashMap::from([(
            "PSX_TEST".into(),
            "value with spaces".into(),
        )]));
        let command = launch(settings, |_| None).unwrap();
        assert_eq!(command.command, "/opt/node/bin/node");
        assert_eq!(
            command.args,
            vec![
                "/workspace/PSX checkout/packages/psx-language-server/bin/psx-language-server.mjs",
                "--stdio"
            ]
        );
        assert_eq!(
            command.env,
            vec![("PSX_TEST".into(), "value with spaces".into())]
        );
    }

    #[test]
    fn path_wrapper_defaults_to_stdio_but_explicit_empty_arguments_are_preserved() {
        let command = launch(LspSettings::default(), |name| {
            assert_eq!(name, SERVER);
            Some("/a workspace/bin/psx-language-server".into())
        })
        .unwrap();
        assert_eq!(command.command, "/a workspace/bin/psx-language-server");
        assert_eq!(command.args, vec!["--stdio"]);
        let settings =
            serde_json::from_str(r#"{"binary":{"path":"/custom/server","arguments":[]}}"#).unwrap();
        assert!(launch(settings, |_| None).unwrap().args.is_empty());
    }

    #[test]
    fn missing_or_empty_command_has_configuration_guidance() {
        let error = launch(LspSettings::default(), |_| None).err().unwrap();
        assert!(error.contains("lsp.psx-language-server.binary.path"));
        let settings = serde_json::from_str(r#"{"binary":{"path":" "}}"#).unwrap();
        assert!(launch(settings, |_| None).is_err());
    }
}

use zed_extension_api::{self as zed, Result, settings::LspSettings};

const SERVER: &str = "psx-language-server";

// Only translates Zed settings to its process ABI; PSX owns all server behavior.
fn launch(
    settings: LspSettings,
    which: impl FnOnce(&str) -> Option<String>,
) -> Result<zed::Command> {
    let binary = settings.binary;
    let command = binary.as_ref().and_then(|b| b.path.clone())
        .or_else(|| which(SERVER))
        .filter(|path| !path.trim().is_empty())
        .ok_or_else(|| format!("PSX server not configured. Set lsp.{SERVER}.binary.path to Node and binary.arguments to [\"/absolute/path/to/psx/packages/psx-language-server/bin/psx-language-server.mjs\", \"--stdio\"], or put a {SERVER} wrapper on PATH. Build the PSX checkout first (npm ci && npx spago build)."))?;
    let args = binary
        .as_ref()
        .and_then(|b| b.arguments.clone())
        .unwrap_or_else(|| vec!["--stdio".into()]);
    let env = binary
        .and_then(|b| b.env)
        .unwrap_or_default()
        .into_iter()
        .collect();
    Ok(zed::Command { command, args, env })
}

struct Psx;

impl zed::Extension for Psx {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        launch(LspSettings::for_worktree(SERVER, worktree)?, |name| {
            worktree.which(name)
        })
    }

    fn language_server_initialization_options(
        &mut self,
        _id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<Option<serde_json::Value>> {
        Ok(LspSettings::for_worktree(SERVER, worktree)?.initialization_options)
    }
}

zed::register_extension!(Psx);
