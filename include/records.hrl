-type id() :: binary().
-type email() :: binary().

-record(user, {
  user_id :: id(),
  email :: email()
}).

-type image_tag() :: binary().

-type image_props() :: [{binary(), any()}].

%% -record(image, {
%%   id :: id(),
%%   title :: binary(),
%%   comment :: binary(),
%%   tags :: [image_tag()],
%%   url :: binary(),
%%   origin :: binary(),
%%   referrer :: binary(),
%%   added_at :: binary(),
%%   width :: integer(),
%%   height :: integer()
%% }).

-type json() :: binary().
-type image() :: json().

-type tag() :: binary().
