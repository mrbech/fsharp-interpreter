run:
	docker-compose run interpreter dotnet watch --project micro-ml run -- ${program}
fsi:
	docker-compose run interpreter dotnet fsi
test:
	docker-compose run interpreter dotnet watch --project tests test
