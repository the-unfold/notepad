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

curl --request POST \
  --url http://localhost:8080/notes/create \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "content": "I am a simple note 2" },
      "uuid": "550e8400-e29b-41d4-a726-446655440002"
    }'

#  Bonus users
curl --request POST \
  --url http://localhost:8080/users \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "email": "john@galt.com0" },
      "uuid": "550e8400-e29b-41d4-a726-446655440010"
    }'

curl --request POST \
  --url http://localhost:8080/users \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "email": "john@galt.com1" },
      "uuid": "550e8400-e29b-41d4-a726-446655440011"
    }'

curl --request POST \
  --url http://localhost:8080/users \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "email": "john@galt.com2" },
      "uuid": "550e8400-e29b-41d4-a726-446655440012"
    }'

curl --request POST \
  --url http://localhost:8080/users \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "email": "john@galt.com3" },
      "uuid": "550e8400-e29b-41d4-a726-446655440013"
    }'

curl --request POST \
  --url http://localhost:8080/users \
  --header 'Content-Type: application/json' \
  --data '{
      "payload": { "email": "john@galt.com4" },
      "uuid": "550e8400-e29b-41d4-a726-446655440014"
    }'
