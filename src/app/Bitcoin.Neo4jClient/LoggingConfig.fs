module BitcoinFs.Neo4jEtl.LoggingConfig
open NLog
open NLog.Layouts
open NLog.Targets
open NLog.Config

let configureLogs useConsole useFile mainLogLevel debugLoggers =

    let config = new LoggingConfiguration()

    let layout = new SimpleLayout(@"${date:yyyy-MM-ddTHH\:mm\:ss} ${logger} ${message}")

    if useConsole then
        let consoleTarget = new ColoredConsoleTarget()
        config.AddTarget("console", consoleTarget)
        consoleTarget.Layout <- layout

        let consoleRule = new LoggingRule("*", mainLogLevel, consoleTarget)
        config.LoggingRules.Add(consoleRule)
        for logger in debugLoggers do
            let consoleRule = new LoggingRule(logger, LogLevel.Debug, consoleTarget)
            config.LoggingRules.Add(consoleRule)


    if useFile then
        let fileTarget = new FileTarget()
        config.AddTarget("file", fileTarget)

        fileTarget.FileName <- new SimpleLayout("${basedir}/log.txt")
        fileTarget.Layout <- layout

        let fileRule = new LoggingRule("*", mainLogLevel, fileTarget)
        config.LoggingRules.Add(fileRule)
        for logger in debugLoggers do
            let fileRule = new LoggingRule(logger, LogLevel.Debug, fileTarget)
            config.LoggingRules.Add(fileRule)


    LogManager.Configuration <- config