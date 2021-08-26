# Because of codegen and compile-time postgres connections, we cant't simply ask docker-compose to build everything.
# Instead, we need a step-by step approach (almost like in GitHub Actions, but with local specifics).

# Make sure we have everything we need, and will not wait for something to be downloaded
docker-compose -f dc.prod-local.yml pull

# Start database for compile-time connections (migrations are applied via volumes)
docker-compose -f dc.prod-local.yml up -Vd postgres

# Build backend
docker-compose -f dc.prod-local.yml build backend



# ...Upcoming codegen steps:

# - get previous stack build cache (only for CI), extract it as a directory among other
# - copy stack cache to backend_artifacts before build starts
# - build (and tag locally) backend_artifacts

# - extract and store stack cache
# - pack server binary to a new image

# - linting all the frontend code with elm-review and elm-format
# - remove generated frontend code 
# - extract generated elm files from backend_artifacts and put them to the frontend codebase
# - format and autofix the generated code
# - run frontend tests
# - compile and pack frontend



# Build frontend
docker-compose -f dc.prod-local.yml build frontend

# Stop the database and everything else
docker-compose -f dc.prod-local.yml down -v
