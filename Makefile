.PHONY: test
test:
	rebar3 do xref,dialyzer
	rebar3 eunit
	rebar3 ct --verbose --readable=true
	rebar3 proper
	rebar3 cover -v

