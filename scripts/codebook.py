# -*- coding: utf-8 -*-


codebook = {
    'SEX': {
        1: 'Female',
        2: 'Male'
    },
    'GENDER_IDENTITY': {
        1: 'Cis man',
        2: 'Cis woman',
        3: 'Trans man (FTM)',
        4: 'Trans woman (MTF)',
        5: 'Trans GNB (Gender Non-Binary)',
        6: 'Trans not specified'
    },
    'TRANS_CIS': {
        1: 'Trans',
        2: 'Cis'
    },
    'Q105': {
        '1': 'About once every 6 months',
        '2': 'About once a year',
        '3': 'About once every 2-3 years',
        '4': 'About once every 4-5 years',
        '5': 'About once every 6 years or less often',
        '6': 'I\'ve never been tested for STIs',
        '7': 'I\'m HIV positive'
    },
    'Q106': {
        '1': 'About once every 1-3 months',
        '2': 'About once every 6 months',
        '3': 'About once a year',
        '4': 'About once every 2 years or less often',
        '5': 'I would only get tested if I felt I was at risk',
        '6': 'I\'ve never been tested for HIV',
        '7': 'I\'m HIV-positive'
    },
    'Q107': {
        '1': 'Never',
        '2': 'Sometimes',
        '3': 'Often',
        '4': 'Always',
        '5': 'Does not apply to me',
        '7': 'Planned missing'
    },
    'Q108': {
        '1': 'Very unlikely',
        '2': 'Unlikely',
        '3': 'Somewhat unlikely',
        '4': 'Likely',
        '5': 'Very likely',
        '6': 'Does not apply to me',
        '7': 'Planned missing'
    },
    'Q109': {
        '1': 'Yes',
        '2': 'No',
        '7': 'Planned missing'
    },
    'Q110': {
        '1': 'Not at all familiar',
        '2': 'Somewhat familiar',
        '3': 'Very familiar'
    },
    'Q111': {
        '1': 'I am against it',
        '2': 'I have mixed feelings about it',
        '3': 'I am for it',
        '4': 'I don\'t have an opinion',
        '5': 'I don\'t know enough about it'
    }
}

# formatted for plotly express bar graph titles
questions = {
    'Q105': 'About how often do you get tested for STIs other than HIV?',
    'Q106': 'About how often do you get tested for HIV?',
    'Q107': 'How often do you worry that you might get HIV?',
    'Q108': 'How likely is it that you will become HIV-positive in your lifetime?',
    'Q109': 'Are you currently taking Truvada as pre-exposure prophylaxis (PrEP)?',
    'Q110': 'Truvada is a pill that HIV negative people can take to prevent HIV infection.<br>This is called PrEP. How familiar are you with Truvada as PrEP?',
    'Q111': 'Are you for or against HIV-negative people taking Truvada as PrEP to prevent the transmission of HIV?'
}