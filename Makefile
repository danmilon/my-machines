install:
	ANSIBLE_LIBRARY=./modules \
		ansible-playbook \
		-K \
		-i hosts \
		--limit $(MACHINE) \
		-c local $(ANSIBLE_ARGS) \
		playbook.yml
