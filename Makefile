
install:
	ansible-playbook -e "ansible_python_interpreter=/usr/bin/python2" \
		-i hosts --limit $(MACHINE) -c local playbook.yml
