install:
	ANSIBLE_LIBRARY=./modules \
		ansible-playbook \
		-K \
		-i hosts \
		--limit $(shell hostname) \
		-c local $(ANSIBLE_ARGS) \
		playbook.yml
