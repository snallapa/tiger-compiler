val arg1 = hd(CommandLine.arguments());
CM.make "sources.cm";
Main.compile arg1;
val _ = OS.Process.exit(OS.Process.success)
