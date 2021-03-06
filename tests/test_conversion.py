import sqlite3
import os


def test_conversion():
    root = os.path.dirname(os.path.abspath(__file__))
    db = os.path.join(root, "ImplicitStateVendingMachine.db")
    conn = sqlite3.connect(db)
    c = conn.cursor()
    c.execute("SELECT * FROM instance")
    insts = c.fetchall()
    assert len(insts) == 2
    c.execute("SELECT * FROM breakpoint")
    bps = c.fetchall()
    assert len(bps) > 10
    conn.close()

