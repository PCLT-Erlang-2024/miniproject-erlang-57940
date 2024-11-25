rm *.beam 2> /dev/null
erlc *.erl || exit 1
escript main.erl
