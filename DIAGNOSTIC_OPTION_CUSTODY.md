# Provider loading and caller options

An explicit selected-parameter request may load posterior and its dependencies. In the reviewed cli 3.6.6 implementation, the first load installs `callr.condition_handler_cli_message` when the option is absent ([source](https://raw.githubusercontent.com/r-lib/cli/v3.6.6/R/onload.R)).

The diagnostic profile removes only that recognized option value when the operation began with both the option and cli namespace absent, cli has subsequently loaded, and the current value still equals cli's exact default handler. Preexisting values, a previously loaded cli namespace and distinct intervening caller values are preserved. The handler is never called by cleanup. Error and interrupt exits retain their original conditions.

This is a narrow known first-load side-effect transaction. It does not reset all caller options, unload namespaces, modify installed bindings or promise to undo arbitrary external load hooks. HMC-only requests do not load this parameter provider. Passive reports do not load any diagnostic provider.
