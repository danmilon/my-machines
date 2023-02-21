install:
	ANSIBLE_LIBRARY=./modules \
		poetry run ansible-playbook \
		-K \
		-i hosts \
		--limit $(shell hostname) \
		-c local $(ANSIBLE_ARGS) \
		playbook.yml
