curl --request POST \
  --url http://localhost:8080/events \
  --header 'Content-Type: application/json' \
  --data '{
      "email": "john@galt.com",
      "uuid": "550e8400-e29b-41d4-a726-446655440000"
    }'