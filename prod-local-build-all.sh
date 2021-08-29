# exit on the first failure
set -e

# Because of codegen and compile-time postgres connections, we cant't simply ask docker-compose to build everything.
# Instead, we need a step-by step approach (almost like in GitHub Actions, but with local specifics).

# Make sure we have everything we need, and will not wait for something to be downloaded
docker-compose -f dc.prod-local.yml pull postgres

# Start database for compile-time connections (migrations are applied via volumes)
docker-compose -f dc.prod-local.yml up -Vd postgres

# Build backend-artifacts
docker-compose -f dc.prod-local.yml build backend-artifacts

# Create container from image without running it to extract artifacts
id=$(docker create notepad_backend-artifacts)

# Save stack depenencies cache
# TODO: check md5 sum before extracting 
# to avoid copying the same archive to the host again (useful only for frequent local docker builds)
docker cp $id:/root/stack_dependencies_cache.tar.gz - > ./backend/cache/stack_dependencies_cache.tar.gz.tar

# Replace the existing generated code with just generated
# Note: we could also do it via docker, 
# but it will require anyone to build backend-artifacts before building the frontend
# which will take 20-40 minutes.
rm -rf frontend/src/Api
docker cp $id:/frontend/src/Api - > frontend/src/Api.tar
tar -xf frontend/src/Api.tar -C frontend/src
rm frontend/src/Api.tar

# Remove the container after extracting artifacts
docker rm -v $id

# Build backend
docker-compose -f dc.prod-local.yml build backend

# - pack server binary to a new image

# - linting all the frontend code with elm-review and elm-format
# - remove generated frontend code 
# - extract generated elm files from backend-artifacts and put them to the frontend codebase
# - format and autofix the generated code
# - run frontend tests
# - compile and pack frontend

# Build frontend
docker-compose -f dc.prod-local.yml build frontend

# Stop the database and everything else
docker-compose -f dc.prod-local.yml down -v
