-module(esli_http).
-export([build/3, build/2]).

build (code, 404, Req) ->
  build({404, <<"Not Found">>, [{content_type, <<"text/html">>}], <<"<center><h1>404 Not Found</h1></center>">>}, Req).

build ({Code, Headers, Content}, Req) ->
  build({Code, <<"OK">>, Headers, Content}, Req);
build ({Code, CodeDescription, Headers, Content}, _Req) ->
  CodeBinary = integer_to_binary(Code),
  Header = <<"HTTP/1.1 ", CodeBinary/binary, " ", CodeDescription/binary>>,
  Length = integer_to_binary(erlang:byte_size(Content)),
  Headers2 = [{content_length, Length} | Headers],
  Header2 = add_header(Headers2, Header),
  <<Header2/binary, "\r\n\r\n", Content/binary>>.


add_header ([], Header) -> Header;
add_header ([{content_type, ContentType} | T], Header) ->
  add_header(T, append(Header, <<"Content-Type">>, ContentType));
add_header ([{content_length, Length} | T], Header) ->
  add_header(T, append(Header, <<"Content-Length">>, Length));
add_header (_, Header) -> Header.


append (Header, Key, Value) ->
  <<Header/binary, "\r\n", Key/binary, ": ", Value/binary>>.
