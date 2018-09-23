CFLAGS+=-std=c99 -O3

prd-trial: prd-trial.c prd.h
	$(CC) $(CFLAGS) prd-trial.c -o prd-trial

depend:
	@make -C .. depend

clean:
	git clean --force -x

.PHONY: clean depend
