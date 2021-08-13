curl --request POST \
  --url http://localhost:8080/users \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "email": "john@galt.com" },
      "uuid": "550e8400-e29b-41d4-a726-446655440000"
    }'

curl --request POST \
  --url http://localhost:8080/notes/create \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "content": "I am a simple note" },
      "uuid": "550e8400-e29b-41d4-a726-446655440001"
    }'