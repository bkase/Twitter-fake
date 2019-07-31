open Core;
open Async;
module Graphql = Graphql_async;

module Tweet = {
  open Graphql.Schema;

  type t('a) = {
    message: string,
    author: string,
    timestamp: 'a,
  };

  module Input0 = {
    type nonrec t = t(unit);
  };

  module Output0 = {
    type nonrec t = t(Time.t);
  };

  module Fields = {
    let message = () =>
      field(
        "message",
        ~typ=non_null(string),
        ~doc="The message inside of the tweet",
        ~args=Arg.[],
        ~resolve=(_, tweet) =>
        tweet.message
      );

    let author = () =>
      field(
        "author",
        ~typ=non_null(string),
        ~doc="The author of the tweet",
        ~args=Arg.[],
        ~resolve=(_, tweet) =>
        tweet.author
      );

    let timestamp = () =>
      field(
        "timestamp",
        ~typ=non_null(string),
        ~doc=
          "Time in number of milliseconds since January 1, 1970 (Unix time)",
        ~args=Arg.[],
        ~resolve=(_, tweet) =>
        Time.to_string(tweet.timestamp)
      );
  };

  module Args = {
    let message = () =>
      Arg.(
        arg(
          "message",
          ~typ=non_null(string),
          ~doc="The message inside of the tweet",
        )
      );

    let author = () =>
      Arg.(
        arg("author", ~typ=non_null(string), ~doc="The author of the tweet")
      );
  };

  module Input = {
    open Input0;
    open Arg;

    let typ: unit => arg_typ(option(t)) =
      () =>
        Arg.(
          obj(
            "TweetInput",
            ~doc="Tweet input",
            ~coerce=(message, author) => {message, author, timestamp: ()},
            ~fields=[Args.message(), Args.author()],
          )
        );
  };

  module Output = {
    open Output0;

    let typ: unit => typ(_, option(t)) =
      () =>
        obj("TweetOutput", ~doc="Tweet fully filled out", ~fields=_ =>
          [Fields.message(), Fields.author(), Fields.timestamp()]
        );
  };
};

let model: ref(list(Tweet.Output0.t)) = ref([]);

module Mutation = {
  open Graphql.Schema;

  module Input = {
    open Arg;
    let send_tweet = () =>
      obj(
        "SendTweetInput",
        ~coerce=Fn.id,
        ~fields=[
          Arg.arg(
            "tweet",
            ~typ=non_null(Tweet.Input.typ()),
            ~doc="The tweet to send",
          ),
        ],
      );
  };

  module Payload = {
    let send_tweet = () =>
      obj("SendTweetPayload", ~fields=_ =>
        [
          field(
            "tweet",
            ~typ=non_null(Tweet.Output.typ()),
            ~doc="The tweet that was sent",
            ~args=Arg.[],
            ~resolve=_ =>
            Fn.id
          ),
        ]
      );
  };

  let send_tweet = () =>
    field(
      "sendTweet",
      ~doc="Send a tweet to the feed",
      ~args=Arg.[arg("input", ~typ=non_null(Tweet.Input.typ()))],
      ~typ=non_null(Payload.send_tweet()),
      ~resolve=(_, (), tweet_in) => {
        let tweet_out = {
          Tweet.message: tweet_in.Tweet.message,
          author: tweet_in.Tweet.author,
          timestamp: Time.now(),
        };
        model := [tweet_out, ...model^];
        tweet_out;
      },
    );
};

module Typ = {
  open Graphql.Schema;
  let feed: unit => field(_, unit) =
    () =>
      field(
        "feed",
        ~doc="Feed of tweets in the system",
        ~args=Arg.[],
        ~typ=non_null(list(non_null(Tweet.Output.typ()))),
        ~resolve=(_, ()) =>
        model^
      );
};

let schema = () =>
  Graphql.Schema.(schema([Typ.feed()], ~mutations=[Mutation.send_tweet()]));

module Graphql_cohttp_async =
  Graphql_cohttp.Make(Graphql.Schema, Cohttp_async.Io, Cohttp_async.Body);

let graphql_callback = Stupid_hack.make_callback(_req => (), schema());

let server =
  Cohttp_async.(
    Server.create_expert(
      ~on_handler_error=
        `Call(
          (_net, exn) =>
            eprintf(
              "Exception while handling whatever %s\n",
              Exn.to_string(exn),
            ),
        ),
      Tcp.Where_to_listen.(bind_to(All_addresses, On_port(9999))),
      (~body, _sock, req) =>
      switch (Cohttp_async.Request.meth(req)) {
      | `OPTIONS =>
        let h = Cohttp.Header.init();
        let h = Cohttp.Header.add(h, "Access-Control-Allow-Origin", "*");
        let h = Cohttp.Header.add(h, "Access-Control-Allow-Headers", "*");
        let h = Cohttp.Header.add(h, "Access-Control-Allow-Methods", "*");
        Deferred.map(
          ~f=r => `Response(r),
          Server.respond_string(~headers=h, ""),
        );
      | _ => graphql_callback((), req, body)
      }
    )
  );

Deferred.ignore(server);

printf("Starting server\n");

Core.never_returns(Scheduler.go());
