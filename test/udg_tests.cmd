@echo off
erl -pa %~dp0/../ebin -pa %~dp0/ebin -run udg_tests_1 t -noshell -noinput -run erlang halt
erl -pa %~dp0/../ebin -pa %~dp0/ebin -run udg_tests_2 t -noshell -noinput -run erlang halt
