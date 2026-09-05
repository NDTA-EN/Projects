# Project

The goal of this project is to read the numbers of credit cards and assign them to a provider and customer. 

The providers will be the credit card companies such as:
1. __Airplus__
2. __American Express__
3. __Dankort__
4. __Diners Club__
5. __Discover__
6. __JCB__
7. __Laser__
8. __Maestro__
9. __MasterCard__
10. __Visa Debit__
11. __Visa Electron__
12. __Visa Purchasing__
13. __Visa__

In doing this, the project will be able to identify valid and invalid card numbers and create graphs using them.

The credit card numbers will be generated using Microsoft Copilot.

As there was a problem with the name of the file "Card Validation System.py" having whitespaces, It has been renamed it 
to a more pythonic variation "card_validation_system.py". However in refactoring the name, it was created as a new file,
meaning that the main file/finished program to be run is "card_validation_system.py" and not "Card Validation System.py". 


## Arguments to run the program

In order to run the program, you will need to first install the required libraries. 
This can be done by running the following in you terminal: pip install -r requirements.txt

### Argument syntax:
- -f = file name (example: "card_numbers_test")
- -n = number number (example: 4484070000000004)

Afterwards, you can run the program by entering one of the following arguments in your command line:
1. python "card_validation_system.py" -f card_numbers ___or___ python "card_validation_system.py" -f more_numbers
2. python "card_validation_system.py" -n ___followed by any number of digits you can think of___ 

If you chose to run a file, the program will output two graphs. One showing the amount of cards per provider and another 
one showing the amount of valid and invalid cards.

To see the results of the program when run using the file argument, open the results.txt file it creates.

If you would like to view the graphs again, you will have to run the program again using the file argument.

To see more interesting graphs use the card_numbers_test file as it contains differing amounts of providers.