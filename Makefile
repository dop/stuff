all: fetch

.PHONEY: fetch

fetch:
	jbuilder build fetch.exe

run:
	make fetch
	./_build/default/fetch.exe
