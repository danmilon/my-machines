
install:
	ansible-playbook -K -i hosts --limit $(MACHINE) -c local playbook.yml
