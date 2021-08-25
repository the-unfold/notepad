# Because of codegen and compile-time postgres connections, we cant't simply ask docker-compose to build everything.
# Instead, we need a step-by step approach (almost like in GitHub Actions, but with local specifics).

docker-compose -f dc.prod-local.yml pull
docker-compose -f dc.prod-local.yml up -Vd postgres
docker-compose -f dc.prod-local.yml build backend
# ...codegen changes will be here
docker-compose -f dc.prod-local.yml build frontend
docker-compose -f dc.prod-local.yml down -v
