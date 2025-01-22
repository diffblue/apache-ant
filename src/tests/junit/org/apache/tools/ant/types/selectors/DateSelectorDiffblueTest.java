package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Parameter;
import org.apache.tools.ant.types.selectors.DateSelector.TimeComparisons;
import org.junit.Test;

public class DateSelectorDiffblueTest {
  /**
   * Test TimeComparisons new {@link TimeComparisons} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TimeComparisons}
   */
  @Test
  public void testTimeComparisonsNewTimeComparisons() {
    // Arrange and Act
    TimeComparisons actualTimeComparisons = new TimeComparisons();

    // Assert
    assertNull(actualTimeComparisons.getValue());
    assertEquals(-1, actualTimeComparisons.getIndex());
  }

  /**
   * Test {@link DateSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {dateselector date: null compare: equal granularity: 1000}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#toString()}
   */
  @Test
  public void testToString_thenReturnDateselectorDateNullCompareEqualGranularity1000() {
    // Arrange, Act and Assert
    assertEquals("{dateselector date: null compare: equal granularity: 1000}", (new DateSelector()).toString());
  }

  /**
   * Test {@link DateSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {dateselector date: null compare: equal granularity: 1000 pattern: foo}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#toString()}
   */
  @Test
  public void testToString_thenReturnDateselectorDateNullCompareEqualGranularity1000PatternFoo() {
    // Arrange
    DateSelector dateSelector = new DateSelector();
    dateSelector.setPattern("foo");

    // Act and Assert
    assertEquals("{dateselector date: null compare: equal granularity: 1000 pattern: foo}", dateSelector.toString());
  }

  /**
   * Test {@link DateSelector#getMillis()}.
   * <p>
   * Method under test: {@link DateSelector#getMillis()}
   */
  @Test
  public void testGetMillis() {
    // Arrange, Act and Assert
    assertEquals(-1L, (new DateSelector()).getMillis());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code before}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code before}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenBefore_whenParameterValueIsBefore() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.WHEN_KEY);
    parameter.setType("Type");
    parameter.setValue("before");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DateSelector#CHECKDIRS_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link DateSelector#CHECKDIRS_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenCheckdirs_key_whenParameterNameIsCheckdirs_key() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.CHECKDIRS_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DateSelector} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link DateSelector} (default constructor) Millis is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenDateSelector_whenNull_thenDateSelectorMillisIsMinusOne() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    // Act
    dateSelector.setParameters(null);

    // Assert that nothing has changed
    assertEquals(-1L, dateSelector.getMillis());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DateSelector#DATETIME_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link DateSelector#DATETIME_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenDatetime_key_whenParameterNameIsDatetime_key() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.DATETIME_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DateSelector#GRANULARITY_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link DateSelector#GRANULARITY_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenGranularity_key_whenParameterNameIsGranularity_key() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.GRANULARITY_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenName_thenDateSelectorErrorIsInvalidParameterName() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code on}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code on}.</li>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenOn_whenParameterValueIsOn_thenDateSelectorErrorIsNull() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.CHECKDIRS_KEY);
    parameter.setType("Type");
    parameter.setValue("on");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DateSelector#PATTERN_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link DateSelector#PATTERN_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenPattern_key_whenParameterNameIsPattern_key() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.PATTERN_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link Boolean#TRUE} toString.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@link Boolean#TRUE} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenTrueToString_whenParameterValueIsTrueToString() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.CHECKDIRS_KEY);
    parameter.setType("Type");
    parameter.setValue(Boolean.TRUE.toString());
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code yes}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code yes}.</li>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenYes_whenParameterValueIsYes_thenDateSelectorErrorIsNull() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.CHECKDIRS_KEY);
    parameter.setType("Type");
    parameter.setValue("yes");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@code Invalid granularity setting millis}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenDateSelectorErrorIsInvalidGranularitySettingMillis() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.GRANULARITY_KEY);
    parameter.setType("Type");
    parameter.setValue(DateSelector.MILLIS_KEY);
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid granularity setting millis", dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@code Invalid millisecond setting millis}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenDateSelectorErrorIsInvalidMillisecondSettingMillis() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.MILLIS_KEY);
    parameter.setType("Type");
    parameter.setValue(DateSelector.MILLIS_KEY);
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid millisecond setting millis", dateSelector.getError());
    assertEquals(-1L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@link DateSelector#MILLIS_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenDateSelectorErrorIsMillis_key() {
    // Arrange
    DateSelector dateSelector = new DateSelector();
    dateSelector.setError(DateSelector.MILLIS_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertEquals(-1L, dateSelector.getMillis());
    assertEquals(DateSelector.MILLIS_KEY, dateSelector.getError());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>When {@link Parameter} (default constructor) Name is {@link DateSelector#MILLIS_KEY}.</li>
   *   <li>Then {@link DateSelector} (default constructor) Millis is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_whenParameterNameIsMillis_key_thenDateSelectorMillisIsFortyTwo() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DateSelector.MILLIS_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    dateSelector.setParameters(parameters);

    // Assert
    assertNull(dateSelector.getError());
    assertEquals(42L, dateSelector.getMillis());
    assertSame(parameters, dateSelector.getParameters());
  }

  /**
   * Test {@link DateSelector#verifySettings()}.
   * <p>
   * Method under test: {@link DateSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    DateSelector dateSelector = new DateSelector();

    // Act
    dateSelector.verifySettings();

    // Assert
    assertEquals("You must provide a datetime or the number of milliseconds.", dateSelector.getError());
  }

  /**
   * Test {@link DateSelector#verifySettings()}.
   * <p>
   * Method under test: {@link DateSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings2() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("You must provide a datetime or the number of milliseconds.");
    parameter.setType("You must provide a datetime or the number of milliseconds.");
    parameter.setValue("42");

    DateSelector dateSelector = new DateSelector();
    dateSelector.setParameters(parameter);

    // Act
    dateSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter You must provide a datetime or the number of milliseconds.",
        dateSelector.getError());
  }

  /**
   * Test {@link DateSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link DateSelector} (default constructor) Millis is one.</li>
   *   <li>Then {@link DateSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DateSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenDateSelectorMillisIsOne_thenDateSelectorErrorIsNull() {
    // Arrange
    DateSelector dateSelector = new DateSelector();
    dateSelector.setMillis(1L);

    // Act
    dateSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(dateSelector.getError());
  }

  /**
   * Test new {@link DateSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DateSelector}
   */
  @Test
  public void testNewDateSelector() {
    // Arrange and Act
    DateSelector actualDateSelector = new DateSelector();

    // Assert
    assertNull(actualDateSelector.getParameters());
    Location location = actualDateSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDateSelector.getDescription());
    assertNull(actualDateSelector.getError());
    assertNull(actualDateSelector.getProject());
    assertNull(actualDateSelector.getRefid());
    assertEquals(-1L, actualDateSelector.getMillis());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualDateSelector.isReference());
  }
}
