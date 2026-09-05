import sys
import random
import argparse
import re
import matplotlib.pyplot as plt
import numpy as np

customer_list = []

class Customer:
    def __init__(self, firstname, lastname, gender, date_of_birth, address, phone_number, email):
        self.customer_id = random.randint(10000, 99999)
        self.firstname = firstname
        self.lastname = lastname
        self.gender = gender
        self.date_of_birth = date_of_birth
        self.address = address
        self.phone_number = phone_number
        self.email = email
        self.provider = None
        self.card_number = None
        self.valid = None

def process_file(file_name):
    try:
        with open(file_name, 'r') as f:
            card_numbers = f.readlines()
        results = {}

        try:
            for card_number in card_numbers:
                card_number = card_number.strip()
                if not card_number:
                    raise ValueError("Empty line in file.")
                if not card_number.isdigit():
                    raise ValueError(f"Non-digits in file: {card_number}")
                is_valid = luhn_algorithm(card_number)
                card_type = get_card_type(card_number)
                results[card_number] = (is_valid, card_type)
        except Exception as e:
            print(f"Invalid character in file: {file_name} | {e} Please remove and try again.")
        return results
    except FileNotFoundError:
        print(f"File:{file_name} not found.")
        sys.exit(1)

def get_card_type(card_number):
    card_number = card_number.replace(" ", "")
    card_sequence = {
        "Airplus": r"^1[0-9]{14,15}$",
        "American Express": r"^3[47][0-9]{13}$",
        "Dankort": r"^5019[0-9]{12}$",
        "Diners Club": r"^36[0-9]{12}$",
        "Discover": r"^6(?:011|5[0-9]{2})[0-9]{12}$",
        "JCB": r"^3528[0-9]{12}$",
        "Laser": r"^6304[0-9]{14,15}$",
        "Maestro": r"^6759[0-9]{12}$",
        "MasterCard": r"^5[1-5][0-9]{14}$",
        "Visa Debit": r"^4462[0-9]{12}$",
        "Visa Electron": r"^4917[0-9]{12}$",
        "Visa Purchasing": r"^4484[0-9]{12}$",
        "Visa": r"^4(?!462|917|484)[0-9]{12,15}$"
    }
    for card_type, pattern in card_sequence.items():
        if re.match(pattern, card_number):
            return card_type
    return "Unknown"

def luhn_algorithm(card_number):
    def digits_of(n):
        return [int(d) for d in str(n)]
    try:
        if len(card_number) < 12 or len(card_number) > 19:
            raise ValueError("Card number must be between 12 and 19 digits long.")
        digits = digits_of(card_number)
        odd_digits = digits[-1::-2]
        even_digits = digits[-2::-2]
        checksum = sum(odd_digits)
    except ValueError as ex:
        print(f"Error validating card number {card_number}: {ex}")
        return False

    for d in even_digits:
        checksum += sum(digits_of(d * 2))
    return checksum % 10 == 0

def assign_customer_card(customer_list, card_results):
    try:
        card_list = list(card_results.items())
        for y, customer in enumerate(customer_list):
            card_number, (is_valid, card_type) = card_list[y]
            customer.provider = card_type
            customer.card_number = card_number
            customer.valid = is_valid
    except IndexError:
        print("Too few card numbers found!")

def write_results(customer_list, result_file="results.txt"):
    try:
        with open(result_file, 'w') as f:
            f.write("Card Validation Results:\n\n")
            for c in customer_list:
                if c.card_number:
                    f.write(f"ID: {c.customer_id}\nFirst Name: {c.firstname} | Last Name: {c.lastname}\n")
                    f.write(f"Gender: {c.gender} | Date of Birth: {c.date_of_birth}\n")
                    f.write(f"Address: {c.address} | Phone Number: {c.phone_number} | Email: {c.email}\n")
                    f.write(f"Provider: {c.provider} | Card Number: {c.card_number} | Validity: {c.valid}\n\n")
    except IOError:
        print(f"Error writing to file, please try again")
        sys.exit(1)

def provider_graph(result_file="results.txt"):
    num_of_providers = {}
    try:
        with open("results.txt", 'r') as f:
            lines = f.readlines()
            for line in lines:
                if line.startswith("Provider:"):
                    provider = line.split("|")[0].split(":")[1].strip()
                    if provider in num_of_providers:
                        num_of_providers[provider] += 1
                    else:
                        num_of_providers[provider] = 1
    except IOError:
        print(f"Error reading file, please try again.")
        sys.exit(1)

    plt.style.use('_mpl-gallery')
    x = list(num_of_providers.keys())
    y = list(num_of_providers.values())
    fig, ax = plt.subplots()
    ax.bar(x, y, color='lightblue', edgecolor='black')
    ax.set(xlabel='Provider', ylabel='Amount', title='Amount of cards per provider')
    plt.subplots_adjust(top=0.95, bottom=0.05)
    plt.show()

def validated_card_graph(result_file="results.txt"):
    num_of_validated_cards = {}
    try:
        with open("results.txt", 'r') as f:
            lines = f.readlines()
            for line in lines:
                if "Validity" in line :
                    validated_cards = line.split("Validity")[1].strip()
                    if validated_cards in num_of_validated_cards:
                        num_of_validated_cards[validated_cards] += 1
                    else:
                        num_of_validated_cards[validated_cards] = 1
    except IOError:
        print(f"Error reading file, please try again.")
        sys.exit(1)

    plt.style.use('_mpl-gallery')
    x = list(num_of_validated_cards.keys())
    y = list(num_of_validated_cards.values())
    fig, ax = plt.subplots()
    ax.bar(x, y, color='indianred', edgecolor='black')
    ax.set(xlabel='Valid Cards', ylabel='Amount', title='Amount of Validated Cards')
    ax.set_xticks([0, 1])
    ax.set_xticklabels(['Valid', 'Invalid'])
    plt.subplots_adjust(top=0.95, bottom=0.05)
    plt.show()

def main():
    parser = argparse.ArgumentParser(description="Check if a credit card number is valid and identify its provider using Luhn algorithm.")
    parser.add_argument('-n', '--number', type=str, help="Use -n and then enter the credit card number to validate")
    parser.add_argument('-f', '--file', type=str, help="Use -f and then enter the name of the file containing credit card numbers to validate")
    args = parser.parse_args()
    if args.number:
        is_valid = luhn_algorithm(args.number)
        card_type = get_card_type(args.number)
        print(f"[!] Credit card number {args.number} is {'valid' if is_valid else 'invalid'} and provided by {card_type}.")

    elif not args.number and not args.file:
        parser.print_help(sys.stderr)
        sys.exit(1)

    if args.file:
        for _ in range(len(process_file(args.file))):
            forename = f"First Name {_ + 1}"
            surname = f"Last Name {_ + 1}"
            sex = f"Gender {_ + 1}"
            birthdate = f"Date of Birth {_ + 1}"
            residence = f"Address {_ + 1}"
            contact_number = f"Phone Number {_ + 1}"
            e_mail = f"Enter email {_ + 1}"
            c = Customer(forename, surname, sex, birthdate, residence, contact_number, e_mail)
            customer_list.append(c)

    return args

if __name__ == '__main__':

    args = main()
    if args.file:
        results = process_file(args.file)
        assign_customer_card(customer_list, results)
        write_results(customer_list)
        provider_graph()
        validated_card_graph()