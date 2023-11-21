import operator
from functools import reduce

from databuilder.codes import CTV3Code, ICD10Code
from ehrql import case, days, when
from ehrql.tables.beta import tpp as schema


def any_of(conditions):
    return reduce(operator.or_, conditions)


def create_sequential_variables(
    dataset, variable_name_template, events, column, num_variables, sort_column=None
):
    sort_column = sort_column or column
    for index in range(num_variables):
        next_event = events.sort_by(getattr(events, sort_column)).first_for_patient()
        events = events.where(
            getattr(events, sort_column) > getattr(next_event, sort_column)
        )
        variable_name = variable_name_template.format(n=index + 1)
        setattr(dataset, variable_name, getattr(next_event, column))


def _registrations_overlapping_period(start_date, end_date):
    regs = schema.practice_registrations
    return regs.where(
        regs.start_date.is_on_or_before(start_date)
        & (regs.end_date.is_after(end_date) | regs.end_date.is_null())
    )


def practice_registration_as_of(date):
    regs = _registrations_overlapping_period(date, date)
    return regs.sort_by(regs.start_date, regs.end_date).first_for_patient()

def has_a_continuous_practice_registration_spanning(start_date, end_date):
    return _registrations_overlapping_period(start_date, end_date).exists_for_patient()


def date_deregistered_from_all_supported_practices():
    max_dereg_date = schema.practice_registrations.end_date.maximum_for_patient()
    # In TPP currently active registrations are recorded as having an end date of
    # 9999-12-31. We convert these, and any other far-future dates, to NULL.
    return case(
        when(max_dereg_date.is_before("3000-01-01")).then(max_dereg_date),
        default=None,
    )

def most_recent_bmi(*, minimum_age_at_measurement, where=True):
    events = schema.clinical_events
    age_threshold = schema.patients.date_of_birth + days(
        # This is obviously inexact but, given that the dates of birth are rounded to
        # the first of the month anyway, there's no point trying to be more accurate
        int(365.25 * minimum_age_at_measurement)
    )
    return (
        # This captures just explicitly recorded BMI observations rather than attempting
        # to calculate it from height and weight measurements. Investigation has shown
        # this to have no real benefit it terms of coverage or accuracy.
        events.where(events.ctv3_code == CTV3Code("22K.."))
        .where(events.date >= age_threshold)
        .where(where)
        .sort_by(events.date)
        .last_for_patient()
    )



