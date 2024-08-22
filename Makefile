remove-replay-logs:
	fish -c 'rm **/compiled/with-cache/*-replay.log'
	raco setup --pkgs llm-lib llm-doc llm-test
	fish -c 'git add **/compiled/with-cache/*-replay.log'
