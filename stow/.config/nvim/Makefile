SPEC=

RUN=nvim --headless --noplugin -u scripts/minimal_init.vim 

.PHONY: all nvim test watch prepare test_color

nvim:
	@nvim --noplugin -u scripts/minimal_init.vim

test:
ifeq ($(strip $(SPEC)),) # a.k.a. $(SPEC) is empty
	@$(RUN) -c "PlenaryBustedDirectory lua/tests { minimal_init = './scripts/minimal_init.vim' }"
else
	@$(RUN) -c "PlenaryBustedFile $(SPEC)"
endif

watch:
	@echo -e '\nRunning tests on "lua/tests/**/*_spec.lua" when any Lua file on "lua/tests" changes\n'
	@find lua/tests -name '*_spec.lua' \
	  | entr -c make test SPEC=$(SPEC)

test_color:
	@nvim --noplugin "+colorscheme custom" "+edit ./lua/my/luasnip.lua" "+nmap q ZQ" "+highlight"
