with open('analyze_call_hierarchy.py', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace arrow and box drawing characters
replacements = {
    '→': '->',
    '•': '*',
    '├─': '|--',
    '└─': '`--',
    '│': '|'
}

for old, new in replacements.items():
    content = content.replace(old, new)

with open('analyze_call_hierarchy.py', 'w', encoding='utf-8') as f:
    f.write(content)

print('Unicode characters replaced with ASCII')
