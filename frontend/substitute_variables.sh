# We use prefixes like ENV_BACKEND_URL
# to make sure we can safely substitute them via regular expressions
# without touching the generated elm code.
# And we build a distinct object for flags
# to make sure that 'ENV_BACKEND_URL' will not appear in the elm codebase

# ... ENV_BACKEND_URL : 'value' ...
# ... ENV_BACKEND_URL : '' ...
# ... ENV_BACKEND_URL: '' ...
# ... ENV_BACKEND_URL :'' ...
# ... ENV_BACKEND_URL:'' ...

# ... ENV_BACKEND_URL : "value" ...
# ... ENV_BACKEND_URL : "" ...
# ... ENV_BACKEND_URL: "" ...
# ... ENV_BACKEND_URL :"" ...
# ... ENV_BACKEND_URL:"" ...

# ... ENV_BACKEND_URL : `value` ...
# ... ENV_BACKEND_URL : `` ...
# ... ENV_BACKEND_URL: `` ...
# ... ENV_BACKEND_URL :`` ...
# ... ENV_BACKEND_URL:`` ...

# into

# ... ENV_BACKEND_URL : 'http://ya.ru' ...

filename=$1

substitute() {
  local key_name=$1
  local new_value=$2
  echo "Substituting ENV_BACKEND_URL to: $new_value"
  sed -E "s@($key_name([ ]*):([ ]*)([\"'\`])([^\"'\`]*)([\"'\`]))@$key_name: '$new_value'@" $filename >$filename.temp
  mv "$filename.temp" $filename
}

substitute "ENV_BACKEND_URL" $BACKEND_URL
