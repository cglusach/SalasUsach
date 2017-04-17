DIR=$(shell stack path --dist-dir)
POSTGRES_PASSWD_DEV=postgres
POSTGRES_IMAGE=postgres:9.6.1-alpine
SECRET_DEV=test

all:
	stack build

run:
	PGPASS=$(POSTGRES_PASSWD_DEV) SECRET=$(SECRET_DEV) $(DIR)/build/SalasUSACHAPI-exe/SalasUSACHAPI-exe

ghci:
	PGPASS=$(POSTGRES_PASSWD_DEV) SECRET=$(SECRET_DEV) stack ghci

docker-pull:
	docker pull $(POSTGRES_IMAGE)

run-db:
	sudo docker run -p 5432:5432 --rm --name postgres-salas -e POSTGRES_PASSWORD=$(POSTGRES_PASSWD_DEV) -d $(POSTGRES_IMAGE) 
