// This dumb file exists to pass CORS headers

open Core;
open Cohttp_async;
open! Caml;
module Graphql = Graphql_async;

let (>>=) = Io.(>>=);

module Params = {
  type t('a) = {
    query: option(string),
    variables: option(list((string, 'a))),
    operation_name: option(string),
  };
  let empty = {query: None, variables: None, operation_name: None};
  let of_json_body_exn = body =>
    if (body == "") {
      empty;
    } else {
      let json = Yojson.Basic.from_string(body);
      {
        query:
          Yojson.Basic.Util.(
            json |> member("query") |> to_option(to_string)
          ),
        variables:
          Yojson.Basic.Util.(
            json |> member("variables") |> to_option(to_assoc)
          ),
        operation_name:
          Yojson.Basic.Util.(
            json |> member("operationName") |> to_option(to_string)
          ),
      };
    };
  let post_params_exn = (req, body) => {
    let headers = Cohttp.Request.headers(req);
    switch (Cohttp.Header.get(headers, "Content-Type")) {
    | Some("application/json") => of_json_body_exn(body)
    | _ => empty
    };
  };
  let extract = (req, body) =>
    try (
      {
        let params = post_params_exn(req, body);
        switch (params.query) {
        | Some(query) =>
          Ok((
            query,
            (
              params.variables :>
                option(list((string, Graphql_parser.const_value)))
            ),
            params.operation_name,
          ))
        | None => Error("Must provide query string")
        };
      }
    ) {
    | Yojson.Json_error(msg) => Error(msg)
    };
};

let respond_string = (~status, ~body, ()) => {
  let h = Cohttp.Header.init();
  let h = Cohttp.Header.add(h, "Access-Control-Allow-Origin", "*");
  let h = Cohttp.Header.add(h, "Access-Control-Allow-Headers", "*");
  let h = Cohttp.Header.add(h, "Access-Control-Allow-Methods", "*");

  Io.return(
    `Response((
      Cohttp.Response.make(~status, ~headers=h, ()),
      Body.of_string(body),
    )),
  );
};

let static_file_response = path =>
  switch (Assets.read(path)) {
  | Some(body) => respond_string(~status=`OK, ~body, ())
  | None => respond_string(~status=`Not_found, ~body="", ())
  };

let execute_query = (ctx, schema, variables, operation_name, query) =>
  switch (Graphql_parser.parse(query)) {
  | Ok(doc) =>
    Graphql.Schema.execute(schema, ctx, ~variables?, ~operation_name?, doc)
  | Error(e) => Io.return(Error(`String(e)))
  };

let execute_request = (schema, ctx, req, body) =>
  Body.to_string(body)
  >>= (
    body_string =>
      switch (Params.extract(req, body_string)) {
      | Error(err) => respond_string(~status=`Bad_request, ~body=err, ())
      | Ok((query, variables, operation_name)) =>
        execute_query(ctx, schema, variables, operation_name, query)
        >>= (
          fun
          | Ok(`Response(data)) => {
              let body = Yojson.Basic.to_string(data);
              respond_string(~status=`OK, ~body, ());
            }
          | Ok(`Stream(stream)) => {
              Graphql.Schema.Io.Stream.close(stream);
              let body = "Subscriptions are only supported via websocket transport";
              respond_string(~status=`Bad_request, ~body, ());
            }
          | Error(err) => {
              let body = Yojson.Basic.to_string(err);
              respond_string(~status=`Bad_request, ~body, ());
            }
        )
      }
  );

let make_callback = (make_context, schema, _conn, req: Cohttp.Request.t, body) => {
  let req_path = Cohttp.Request.uri(req) |> Uri.path;
  let path_parts = Astring.String.cuts(~empty=false, ~sep="/", req_path);
  let headers = Cohttp.Request.headers(req);
  let accept_html =
    switch (Cohttp.Header.get(headers, "accept")) {
    | None => false
    | Some(s) => List.mem("text/html", String.split_on_char(',', s))
    };
  switch (req.meth, path_parts, accept_html) {
  | (`GET, ["graphql", _], true)
  | (`GET, ["graphql"], true) => static_file_response("index.html")
  | (`GET, ["graphql"], false) =>
    execute_request(schema, make_context(req), req, body)
  | (`POST, ["graphql"], _) =>
    execute_request(schema, make_context(req), req, body)
  | _ => respond_string(~status=`Not_found, ~body="", ())
  };
};
