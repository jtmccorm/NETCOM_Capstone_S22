"""
This script reads in the enriched domain data from NULLFOX and then pre-processes it for anomaly scoring. The result of
the pre-processing are saved to an AWS S3 bucket.

@author Katy Dula
@author Bobby Nelson
@date 5/8/2022
"""

import pandas as pd
from ast import literal_eval
import numpy as np
import json
import datetime
import os

# ----------------------------------------------
# Handcrafted Dictionaries:
# ----------------------------------------------

state_dict = {'Alabama': 'AL',
              'Alaska': 'AK',
              'Arizona': 'AZ',
              'Arkansas': 'AR',
              'California': 'CA',
              'Colorado': 'CO',
              'Connecticut': 'CT',
              'Delaware': 'DE',
              'Florida': 'FL',
              'Georgia': 'GA',
              'Hawaii': 'HI',
              'Idaho': 'ID',
              'Illinois': 'IL',
              'Indiana': 'IN',
              'Iowa': 'IA',
              'Kansas': 'KS',
              'Kentucky': 'KY',
              'Louisiana': 'LA',
              'Maine': 'ME',
              'Maryland': 'MD',
              'Massachusetts': 'MA',
              'Michigan': 'MI',
              'Minnesota': 'MN',
              'Mississippi': 'MS',
              'Missouri': 'MO',
              'Montana': 'MT',
              'Nebraska': 'NE',
              'Nevada': 'NV',
              'New Hampshire': 'NH',
              'New Jersey': 'NJ',
              'New Mexico': 'NM',
              'New York': 'NY',
              'North Carolina': 'NC',
              'North Dakota': 'ND',
              'Ohio': 'OH',
              'Oklahoma': 'OK',
              'Oregon': 'OR',
              'Pennsylvania': 'PA',
              'Rhode Island': 'RI',
              'South Carolina': 'SC',
              'South Dakota': 'SD',
              'Tennessee': 'TN',
              'Texas': 'TX',
              'Utah': 'UT',
              'Vermont': 'VT',
              'Virginia': 'VA',
              'Washington': 'WA',
              'West Virginia': 'WV',
              'Wisconsin': 'WI',
              'Wyoming': 'WY',
              'Ala.': 'AL',
              'Ariz.': 'AZ',
              'Ark.': 'AR',
              'Calif.': 'CA',
              'Colo.': 'CO',
              'Conn.': 'CT',
              'Del.': 'DE',
              'Fla.': 'FL',
              'Ga.': 'GA',
              'Ill.': 'IL',
              'Ind.': 'IN',
              'Kans.': 'KS',
              'Ky.': 'KY',
              'La.': 'LA',
              'Md.': 'MD',
              'Mass.': 'MA',
              'Mich.': 'MI',
              'Minn.': 'MN',
              'Miss.': 'MS',
              'Mo.': 'MO',
              'Mont.': 'MT',
              'Neb.': 'NE',
              'Nebr.': 'NE',
              'Nev.': 'NV',
              'N.H.': 'NH',
              'N.J.': 'NJ',
              'N.Mex.': 'NM',
              'N.Y.': 'NY',
              'N.C.': 'NC',
              'N.Dak.': 'ND',
              'Okla.': 'OK',
              'Ore.': 'OR',
              'Oreg.': 'OR',
              'Pa.': 'PA',
              'R.I.': 'RI',
              'S.C.': 'SC',
              'S.Dak.': 'SD',
              'Tenn.': 'TN',
              'Tex.': 'TX',
              'Vt.': 'VT',
              'Va.': 'VA',
              'Wash.': 'WA',
              'W.Va.': 'WV',
              'Wis.': 'WI',
              'Wisc.': 'WI',
              'Wyo.': 'WY',
              'AL': 'AL',
              'AK': 'AK',
              'AZ': 'AZ',
              'AR': 'AR',
              'CA': 'CA',
              'CO': 'CO',
              'CT': 'CT',
              'DE': 'DE',
              'FL': 'FL',
              'GA': 'GA',
              'HI': 'HI',
              'ID': 'ID',
              'IL': 'IL',
              'IN': 'IN',
              'IA': 'IA',
              'KS': 'KS',
              'KY': 'KY',
              'LA': 'LA',
              'ME': 'ME',
              'MD': 'MD',
              'MA': 'MA',
              'MI': 'MI',
              'MN': 'MN',
              'MS': 'MS',
              'MO': 'MO',
              'MT': 'MT',
              'NE': 'NE',
              'NV': 'NV',
              'NH': 'NH',
              'NJ': 'NJ',
              'NM': 'NM',
              'NY': 'NY',
              'NC': 'NC',
              'ND': 'ND',
              'OH': 'OH',
              'OK': 'OK',
              'OR': 'OR',
              'PA': 'PA',
              'RI': 'RI',
              'SC': 'SC',
              'SD': 'SD',
              'TN': 'TN',
              'TX': 'TX',
              'UT': 'UT',
              'VT': 'VT',
              'VA': 'VA',
              'WA': 'WA',
              'WV': 'WV',
              'WI': 'WI',
              'WY': 'WY'}

country_dict = {'Afghanistan': 'AF',
                'Aland Islands': 'AX',
                'Albania': 'AL',
                'Algeria': 'DZ',
                'American Samoa': 'AS',
                'Andorra': 'AD',
                'Angola': 'AO',
                'Anguilla': 'AI',
                'Antarctica': 'AQ',
                'Antigua and Barbuda': 'AG',
                'Antigua': 'AG',
                'Barbuda': 'AG',
                'Argentina': 'AR',
                'Armenia': 'AM',
                'Aruba': 'AW',
                'Ascension Island': 'AC',
                'Australia': 'AU',
                'Austria': 'AT',
                'Azerbaijan': 'AZ',
                'Bahamas': 'BS',
                'Bahrain': 'BH',
                'Barbados': 'BB',
                'Bangladesh': 'BD',
                'Belarus': 'BY',
                'Belgium': 'BE',
                'Belize': 'BZ',
                'Benin': 'BJ',
                'Bermuda': 'BM',
                'Bhutan': 'BT',
                'Botswana': 'BW',
                'Bolivia': 'BO',
                'Bosnia and Herzegovina': 'BA',
                'Bosnia': 'BA',
                'Herzegovina': 'BA',
                'Bouvet Island': 'BV',
                'Brazil': 'BR',
                'British Indian Ocean Territory': 'IO',
                'Brunei Darussalam': 'BN',
                'Bulgaria': 'BG',
                'Burkina Faso': 'BF',
                'Burundi': 'BI',
                'Cambodia': 'KH',
                'Cameroon': 'CM',
                'Canada': 'CA',
                'Cape Verde': 'CV',
                'Cayman Islands': 'KY',
                'Central African Republic': 'CF',
                'Chad': 'TD',
                'Chile': 'CL',
                'China': 'CN',
                'Christmas Island': 'CX',
                'Cocos Islands': 'CC',
                'Keeling Islands': 'CC',
                'Colombia': 'CO',
                'Comoros': 'KM',
                'Congo': 'CG',
                'Democratic Republic of Congo': 'CD',
                'Cook Islands': 'CK',
                'Costa Rica': 'CR',
                "Cote D'Ivoire ": 'CI',
                'Ivory Coast': 'CI',
                'Croatia': 'HR',
                'Cuba': 'CU',
                'Cyprus': 'CY',
                'Czech Republic': 'CZ',
                'Denmark': 'DK',
                'Djibouti': 'DJ',
                'Dominica': 'DM',
                'Dominican Republic': 'DO',
                'East Timor': 'TP',
                'Ecuador': 'EC',
                'Egypt': 'EG',
                'El Salvador': 'SV',
                'Equatorial Guinea': 'GQ',
                'Eritrea': 'ER',
                'Estonia': 'EE',
                'Ethiopia': 'ET',
                'European Union': 'EU',
                'Falkland Islands': 'FK',
                'Malvinas': 'FK',
                'Faroe Islands': 'FO',
                'Fiji': 'FJ',
                'Finland': 'FI',
                'France': 'FR',
                'French Guiana': 'GF',
                'French Polynesia': 'PF',
                'French Southern Territories': 'TF',
                'North Macedonia': 'MK',
                'Gabon': 'GA',
                'Gambia': 'GM',
                'Georgia': 'GE',
                'Germany': 'DE',
                'Ghana': 'GH',
                'Gibraltar': 'GI',
                'Great Britain': 'GB',
                'Greece': 'GR',
                'Greenland': 'GL',
                'Grenada': 'GD',
                'Guadeloupe': 'GP',
                'Guam': 'GU',
                'Guatemala': 'GT',
                'Guernsey': 'GG',
                'Guinea': 'GN',
                'Guinea-Bissau': 'GW',
                'Guyana': 'GY',
                'Haiti': 'HT',
                'Heard and McDonald Islands': 'HM',
                'Heard Islands': 'HM',
                'McDonald Islands': 'HM',
                'Heard Island': 'HM',
                'McDonald Island': 'HM',
                'Honduras': 'HN',
                'Hong Kong': 'HK',
                'Hungary': 'HU',
                'Iceland': 'IS',
                'India': 'IN',
                'Indonesia': 'ID',
                'Iran': 'IR',
                'Iraq': 'IQ',
                'Ireland': 'IE',
                'Israel': 'IL',
                'Isle of Man': 'IM',
                'Italy': 'IT',
                'Jersey': 'JE',
                'Jamaica': 'JM',
                'Japan': 'JP',
                'Jordan': 'JO',
                'Kazakhstan': 'KZ',
                'Kenya': 'KE',
                'Kiribati': 'KI',
                'North Korea': 'KP',
                'South Korea': 'KR',
                'Kosovo': 'XK',
                'Kuwait': 'KW',
                'Kyrgyzstan': 'KG',
                'Laos': 'LA',
                'Latvia': 'LV',
                'Lebanon': 'LB',
                'Liechtenstein': 'LI',
                'Liberia': 'LR',
                'Libya': 'LY',
                'Lesotho': 'LS',
                'Lithuania': 'LT',
                'Luxembourg': 'LU',
                'Macau': 'MO',
                'Madagascar': 'MG',
                'Malawi': 'MW',
                'Malaysia': 'MY',
                'Maldives': 'MV',
                'Mali': 'ML',
                'Malta': 'MT',
                'Marshall Islands': 'MH',
                'Martinique': 'MQ',
                'Mauritania': 'MR',
                'Mauritius': 'MU',
                'Mayotte': 'YT',
                'Mexico': 'MX',
                'Micronesia': 'FM',
                'Monaco': 'MC',
                'Moldova': 'MD',
                'Mongolia': 'MN',
                'Montenegro': 'YU',
                'Montserrat': 'MS',
                'Morocco': 'MA',
                'Mozambique': 'MZ',
                'Myanmar': 'MM',
                'Namibia': 'NA',
                'Nauru': 'NR',
                'Nepal': 'NP',
                'Netherlands': 'NL',
                'Netherlands Antilles': 'AN',
                'Neutral Zone': 'NT',
                'New Caledonia': 'NC',
                'New Zealand': 'NZ',
                'Nicaragua': 'NI',
                'Niger': 'NE',
                'Nigeria': 'NG',
                'Niue': 'NU',
                'Norfolk Island': 'NF',
                'Northern Mariana Islands': 'MP',
                'Norway': 'NO',
                'Oman': 'OM',
                'Pakistan': 'PK',
                'Palau': 'PW',
                'Occupied Palestinian Territory': 'PS',
                'Palestine': 'PS',
                'Panama': 'PA',
                'Papua New Guinea': 'PG',
                'Paraguay': 'PY',
                'Peru': 'PE',
                'Philippines': 'PH',
                'Pitcairn': 'PN',
                'Poland': 'PL',
                'Portugal': 'PT',
                'Puerto Rico': 'PR',
                'Qatar': 'QA',
                'Reunion': 'RE',
                'Romania': 'RO',
                'Russian Federation': 'RU',
                'Russia': 'RU',
                'Rwanda': 'RW',
                'S. Georgia and S. Sandwich Isls.': 'GS',
                'South Georgia and South Sandwich Islands': 'GS',
                'South Georgia Island': 'GS',
                'South Sandwich Island': 'GS',
                'Saint Helena': 'SH',
                'Saint Kitts and Nevis': 'KN',
                'Saint Lucia': 'LC',
                'Saint Martin': 'MF',
                'Saint Vincent & the Grenadines': 'VC',
                'Samoa': 'WS',
                'San Marino': 'SM',
                'Sao Tome and Principe': 'ST',
                'Saudi Arabia': 'SA',
                'Senegal': 'SN',
                'Serbia': 'YU',
                'Serbia and Montenegro': 'YU',
                'Seychelles': 'SC',
                'Sierra Leone': 'SL',
                'Singapore': 'SG',
                'Slovenia': 'SI',
                'Slovakia': 'SK',
                'Solomon Islands': 'SB',
                'Somalia': 'SO',
                'South Africa': 'ZA',
                'South Sudan': 'SS',
                'Spain': 'ES',
                'Sri Lanka': 'LK',
                'Sudan': 'SD',
                'Suriname': 'SR',
                'Svalbard & Jan Mayen Islands': 'SJ',
                'Svalbard Island': 'SJ',
                'Jan Mayen Island': 'SJ',
                'Swaziland': 'SZ',
                'Sweden': 'SE',
                'Switzerland': 'CH',
                'Syria': 'SY',
                'Taiwan': 'TW',
                'Tajikistan': 'TJ',
                'Tanzania': 'TZ',
                'Thailand': 'TH',
                'Togo': 'TG',
                'Tokelau': 'TK',
                'Tonga': 'TO',
                'Trinidad and Tobago': 'TT',
                'Trinidad': 'TT',
                'Tobago': 'TT',
                'Tunisia': 'TN',
                'Turkey': 'TR',
                'Turkmenistan': 'TM',
                'Turks and Caicos Islands': 'TC',
                'Tuvalu': 'TV',
                'Uganda': 'UG',
                'Ukraine': 'UA',
                'United Arab Emirates': 'AE',
                'United Kingdom': 'UK',
                'United States': 'US',
                'US Minor Outlying Islands': 'UM',
                'Uruguay': 'UY',
                'Uzbekistan': 'UZ',
                'Vanuatu': 'VU',
                'Venezuela': 'VE',
                'Viet Nam': 'VN',
                'Vietnam': 'VN',
                'British Virgin Islands': 'VG',
                'Virgin Islands': 'VI',
                'Wallis and Futuna Islands': 'WF',
                'Wallis Island': 'WF',
                'Futuna Island': 'WF',
                'Western Sahara': 'EH',
                'Yemen': 'YE',
                'Zambia': 'ZM',
                'Zaire': 'ZR',
                'Zimbabwe': 'ZW'}


# ----------------------------------------------
# Helper Functions:
# ----------------------------------------------

# ----------------------------------------------
# CleanTrickyColumns():
# Sub-Helpers:

def getCertAge(df):
    """
    Calculates the age of the cert associated with the domain.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df (Pandas Dataframe): enriched data frame with cert_age field added.
    """
    certs = []
    for idx, x in enumerate(df['CERT']):
        if (x[0] == '"'):
            x = x[1:-1]
        x = x.replace('""', '"').replace("'", '"').replace('t"s', "t's").replace('="', '=').replace('c."',
                                                                                                    'c.').replace('y""',
                                                                                                                  'y"')
        certs.append(json.loads(x))

    allDates = []
    for idx, c in enumerate(certs):
        dates = []
        for x in c:
            dates.append(datetime.datetime.strptime(x['not_before'], "%Y-%m-%dT%H:%M:%S"))
        try:
            allDates.append(min(dates))
        except:
            allDates.append('')

    df['not_before'] = allDates
    # date in single quotes should be the date the report was pulled from the system
    #df['cert_age'] = abs(df['not_before'] - datetime.datetime.strptime('2022-01-30', "%Y-%m-%d"))

    # can use below for productionized data (uses today's date for calculation of length of time cert has been valid
    # WARNING: domain age may not calculated the same, and this may cause issues with interpolation
    df['cert_age'] = abs(df['not_before']-datetime.datetime.today())

    df['cert_age'] = df['cert_age'].dt.days

    return df


def imputeDomainAge(df):
    """
    Imputes the age of the domain for NULL values. If there is a cert, sets the domain age to the oldest cert age.
    If there is no cert, sets the age to zero.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX with cert_age added.
    Returns:
        df (Pandas Dataframe): enriched data frame with WHOIS_DOMAIN_AGE field added.
    """
    df = getCertAge(df)

    # FILL NULLS WITH EARLIEST CERT DATE
    df['WHOIS_DOMAIN_AGE'].fillna(df['cert_age'], inplace=True)

    # FILL NULLS WITH ZERO IF THERE IS NO CERT
    df['WHOIS_DOMAIN_AGE'].fillna(0, inplace=True)

    return df


def createSharedDomainCount(df_clean):
    """
    Calculates the number of shared domains.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df (Pandas Dataframe): enriched data frame with shared_domains_count field added.
    """
    shared_domain_count = []

    for val in df_clean['CERT_SHARED_DOMAINS']:
        if val != '[]':
            cnt = len(literal_eval(val))
            shared_domain_count.append(cnt)
        else:
            cnt = 0
            shared_domain_count.append(cnt)

    df_clean['SHARED_DOMAINS_COUNT'] = shared_domain_count

    return df_clean


def to_list(x):
    try:
        return literal_eval(x)

    except Exception:
        return str(x)


def cleanCountryCode(col_str, df):
    """
    Uses a dictionary of country codes to clean the countries associated with the domains.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
        col_str (str): name of the country code field to clean.
    Returns:
        df (Pandas Dataframe): enriched data frame with cleaned field for country codes.
    """
    df_consistent = df.copy()
    clean_countries = []

    for val in df_consistent[col_str]:

        if val in list(state_dict.values()):
            clean_countries.append("US")

        elif val in list(country_dict.keys()):
            clean_countries.append(country_dict[val])

        elif val == None:
            clean_countries.append("Missing")

        elif val == "nan":
            clean_countries.append("Missing")

        elif val.strip() == 'Personal data, can not be publicly disclosed according to applicable laws.':
            clean_countries.append("undisclosed")

        else:
            clean_countries.append(val)

    col_name = col_str + "_consistent"
    df_consistent[col_name] = clean_countries

    return df_consistent


def cleanWhoisCc(df_clean):
    """
    Cleans the country codes in the data.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df (Pandas Dataframe): enriched data frame with cleaned country codes.
    """
    df_clean['WHOIS_CC'] = df_clean.apply(lambda x: to_list(x['WHOIS_CC']), axis=1)
    df_long = df_clean.explode('WHOIS_CC')
    df_long = cleanCountryCode('WHOIS_CC', df_long)
    df_long.drop("WHOIS_CC", inplace=True, axis=1)
    df_long = pd.get_dummies(df_long, prefix="WHOIS_CC_", prefix_sep='_', columns=['WHOIS_CC_consistent'])
    df_clean = df_long.groupby('id').max().reset_index()

    return df_clean


def cleanSharedDomains(df_clean):
    """
    Cleans the CERT_SHARED_DOMAINS field.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df (Pandas Dataframe): enriched data frame with CERT_SHARED_DOMAINS cleaned.
    """
    df_clean['CERT_SHARED_DOMAINS'] = df_clean.apply(lambda x: literal_eval(x['CERT_SHARED_DOMAINS']), axis=1)
    df_long = df_clean.explode('CERT_SHARED_DOMAINS')

    # deal with this repetition : sniXXXXX.cloudflaressl.com'
    cleaned = []

    for val in df_long['CERT_SHARED_DOMAINS']:
        if type(val) != float:
            if 'cloudflaressl.com' in val:
                new_val = val[(val.find('.') + 1):]
                cleaned.append(new_val)
            else:
                cleaned.append(val)
        else:
            cleaned.append(val)

    df_long['CERT_SHARED_DOMAINS'] = cleaned

    df_long = pd.get_dummies(df_long, prefix="CERT_SHARED_DOMAINS_", prefix_sep='_', columns=['CERT_SHARED_DOMAINS'])

    df_clean = df_long.groupby('id').max().reset_index()

    return df_clean


# CleanTrickyColumns():
# Main:
def cleanTrickyColumns(df):
    """
    Creates and cleans domain information like age and country codes.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df (Pandas Dataframe): enriched data frame with domain features added.
    """
    # make ID col for exploding and re-grouping:
    df_clean = df
    df_clean['id'] = df.index

    df_clean = createSharedDomainCount(df_clean)

    df_clean = cleanWhoisCc(df_clean)

    df_clean = cleanSharedDomains(df_clean)

    df_clean = imputeDomainAge(df_clean)

    return df_clean


# ----------------------------------------------
# BinarizeEm():

def BinarizeColumns(df):
    """
    Convert fields to binary.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX after cleanTrickyColumns().
    Returns:
        df_clean (Pandas Dataframe): enriched data frame with binary fields added.
    """
    df_clean = df
    df_clean['HAS_RECORD_DNS_MX'] = np.where(df_clean['RECORD_DNS_MX'] != '[]', 1, 0)
    df_clean['HAS_CERT_SHARED_DOMAINS'] = np.where(df_clean['CERT_SHARED_DOMAINS'] != '[]', 1, 0)

    return df_clean


# ----------------------------------------------
# VanillaOHE():
# sub-helpers:

def cleanSOA(df):
    """
    Clean the SOA data.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df_clean (Pandas Dataframe): enriched data frame with CERT_SHARED_DOMAINS updated with SOA values.
    """
    cleaned = []
    df_clean = df

    for val in df_clean['RECORD_DNS_SOA']:

        if 'cloudflaressl.com' in val:
            new_val = val[(val.find('.') + 1):]
            cleaned.append(new_val)
        else:
            cleaned.append(val)

    df_clean['CERT_SHARED_DOMAINS'] = cleaned

    return df_clean


# VanillaOHE():
# Main:

def vanillaOHE(df):
    """
    One hot encode the necessary fields.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX.
    Returns:
        df (Pandas Dataframe): enriched data frame with one-hot-encoded categorical fields.
    """
    df['CERT_AUTHORITY'] = df['CERT_AUTHORITY'].replace(np.nan, 'missing')
    df['WHOIS_REGISTRAR'] = df['WHOIS_REGISTRAR'].replace(np.nan, 'missing')

    df = pd.get_dummies(df, prefix="CERT_AUTHORITY_", prefix_sep='_', columns=['CERT_AUTHORITY'])
    df = pd.get_dummies(df, prefix="WHOIS_REGISTRAR_", prefix_sep='_', columns=['WHOIS_REGISTRAR'])

    df = cleanSOA(df)
    df = pd.get_dummies(df, prefix="RECORD_DNS_SOA_", prefix_sep='_', columns=['RECORD_DNS_SOA'])

    return df


# ----------------------------------------------
# DropEm():

def dropColumns(df):
    """
    Drop unwanted columns for the anomaly detection.

    Parameters:
        df (Pandas Dataframe): enriched data frame from NULLFOX after all extra fields are added.
    Returns:
        df_clean (Pandas Dataframe): enriched data frame without the unwanted fields.
    """
    df_clean = df
    # ------------
    # These two columns may not be present in the ACTUAL enriched data file!
    df_clean.drop('Unnamed: 0', inplace=True, axis=1)
    df_clean.drop('Unnamed: 0.1', inplace=True, axis=1)
    # -------------
    df_clean.drop('INDICATOR', inplace=True, axis=1)
    df_clean.drop('GN_IP_SRC', inplace=True, axis=1)
    df_clean.drop('most_recent_hit_date', inplace=True, axis=1)
    df_clean.drop('CERT', inplace=True, axis=1)
    df_clean.drop('RECORD_DNS_A', inplace=True, axis=1)
    df_clean.drop('RECORD_DNS_NS', inplace=True, axis=1)
    df_clean.drop('RECORD_DNS_AAAA', inplace=True, axis=1)
    df_clean.drop('RECORD_DNS_MX', inplace=True, axis=1)
    df_clean.drop('RECORD_DNS_TXT', inplace=True, axis=1)
    df_clean.drop('WHOIS_CREATION_DATE', inplace=True, axis=1)
    df_clean.drop('cert_age', inplace=True, axis=1)
    df_clean.drop('not_before', inplace=True, axis=1)
    df_clean.drop('id', inplace=True, axis=1)

    return df_clean


# ----------------------------------------------
# NormalizeEm():
# Sub-helper:

def normalize(cols, df):
    '''
  input:
  cols = list of strings of column names in df that you want to normalize
  df = X in dataframe
  output:
  normalized dataframe
  '''
    df_min_max_scaled = df.copy()

    for column in cols:
        df_min_max_scaled[column] = (df_min_max_scaled[column] - df_min_max_scaled[column].min()) / (
                    df_min_max_scaled[column].max() - df_min_max_scaled[column].min())

    return df_min_max_scaled


# NormalizeEm():
# Main:
def normalizeEm(df_clean):
    """
    Normalize the necessary columns for anomaly detection (values squashed from 0 to 1).

    Parameters:
        df_clean (Pandas Dataframe): enriched data frame from NULLFOX with all fields needed for anomaly detection.
    Returns:
        df_clean (Pandas Dataframe): enriched data frame with normalized columns.
    """
    norm_cols = ['virus_total_mal_count', 'virus_total_sus_count', 'VT_SCORE', 'count', \
                 'unique_IP_count', 'CERT_COUNT', 'CERT_VALID_DAYS', 'NUM_ACTIVE_CERTS', \
                 'NUM_EXPIRED_CERTS', 'WHOIS_DOMAIN_AGE']

    df_clean = normalize(norm_cols, df_clean)

    return df_clean


def getFromS3(bucket_name, object_key):
    """
    Retrieve a data frame from a CSV stored on AWS S3.

    Parameters:
        bucket_name (str): name of the S3 bucket.
        object_key (str): object key for the AWS S3 object.
    Returns:
        df (Pandas Dataframe): pandas dataframe of the CSV.
    """
    path = "s3://{}/{}".format(bucket_name, object_key)

    df = pd.read_csv(path)
    return df


def writeToS3(df, bucket_name, object_key):
    """
        Writes a pandas dataframe to AWS S3.

        Parameters:
            df (Pandas Dataframe): pandas dataframe to write to a CSV.
            bucket_name (str): name of the S3 bucket.
            object_key (str): object key for the AWS S3 object.
        """
    path = "s3://{}/{}".format(bucket_name, object_key)

    df.to_csv(path, index=False)


# ----------------------------------------------
# Main:
# ----------------------------------------------
def main():
    # TODO: change these resource strings to reflect the output of NULLFOX.
    df = getFromS3(bucket_name="xstream-capstone",
                   object_key="data/enriched_altIP.csv")

    df = BinarizeColumns(df)

    df = vanillaOHE(df)

    df = cleanTrickyColumns(df)

    df = dropColumns(df)

    df = normalizeEm(df)

    # TODO: change these resource strings to reflect where you want to save the anomaly detection results.
    writeToS3(df=df,
              bucket_name="xstream-capstone",
              object_key="data/CleanEnrichedData.csv")


if __name__ == '__main__':
    main()
