import sqlite3
import os
import subprocess
import tempfile


def get_db(temp, db_name):
    # tests/
    tests = os.path.dirname(os.path.abspath(__file__))
    vectors = os.path.join(tests, "vectors")
    root = os.path.dirname(tests)
    db = os.path.join(temp, db_name + ".db")
    # need to convert from firrtl to db
    firrtl = os.path.join(root, "bin", "firrtl2toml")
    fir_file = os.path.join(vectors, db_name + ".fir")
    toml_file = os.path.join(temp, db_name + ".toml")
    subprocess.check_call([firrtl, fir_file, toml_file], env=os.environ)
    # db file
    subprocess.check_call(["toml2hgdb", toml_file, db], env=os.environ)
    return db


def test_vending_machine_conversion():
    with tempfile.TemporaryDirectory() as temp:
        db = get_db(temp, "ImplicitStateVendingMachine")
        conn = sqlite3.connect(db)
        c = conn.cursor()
        c.execute("SELECT * FROM instance")
        insts = c.fetchall()
        assert len(insts) == 2
        c.execute("SELECT * FROM breakpoint")
        bps = c.fetchall()
        assert len(bps) > 10
        conn.close()


def test_reg_conversion():
    with tempfile.TemporaryDirectory() as temp:
        db = get_db(temp, "REG")
        conn = sqlite3.connect(db)
        c = conn.cursor()
        c.execute("SELECT * FROM assignment ")
        res = c.fetchall()
        assert len(res) > 16
