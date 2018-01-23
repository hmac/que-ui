docker:
	docker build -t que-ui .

client:
	cd client && yarn build

update_ekg_assets:
	find ~/.stack -name "assets" -exec cp -a {} assets \;
