from os import getcwd
from os.path import join

from psycopg2 import connect

def create_eia_table(cur, name):
    query = ('CREATE TABLE IF NOT EXISTS {} (msn char(7), yyyymm char(6),' +
             'value real, column_order char(2), description varchar(90),' +
             'unit varchar(41));')
    cur.execute(query.format(name))

def import_csv(cur, csv_file, table):
    cur.execute("COPY {} FROM %s NULL AS 'Not Available' CSV HEADER"
                .format(table), (csv_file, ))

tables = {'eia_elec': 'EIA_CO2_Electric_2014.csv',
          'eia_trans': 'EIA_CO2_Transportation_2014.csv',
          'eia_mkwh': 'EIA_MkWh_2014.csv'}

if __name__ == '__main__':
    with connect('dbname=ecs165_project user=postgres') as conn:
        with conn.cursor() as cur:
            for table, path in tables.iteritems():
                create_eia_table(cur, table)
                import_csv(cur, join(getcwd(), path), table)
