from setuptools import setup, find_packages
from ASnake import ASnakeVersion

if __name__ == '__main__':
    setup(
    name='ASnake',
    version=ASnakeVersion[1:],
    author='Ahri Fox',
    url='https://asnake.org',
    description='Optimizing Python transpiler for the ASnake programming language.',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    packages=find_packages(),
    install_requires=[
        'sly>=0.4',
        'autopep8>=1.5.5',
    ],
    entry_points={
        'console_scripts': [
            'ASnake=ASnake:main',
        ],
    },
    python_requires='>=3.6',
)
