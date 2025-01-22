package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Parameter;
import org.apache.tools.ant.types.selectors.SizeSelector.ByteUnits;
import org.apache.tools.ant.types.selectors.SizeSelector.SizeComparisons;
import org.junit.Test;

public class SizeSelectorDiffblueTest {
  /**
   * Test ByteUnits {@link ByteUnits#getValues()}.
   * <p>
   * Method under test: {@link ByteUnits#getValues()}
   */
  @Test
  public void testByteUnitsGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"K", "k", "kilo", "KILO", "Ki", "KI", "ki", "kibi", "KIBI", "M", "m", "mega", "MEGA",
        "Mi", "MI", "mi", "mebi", "MEBI", "G", "g", "giga", "GIGA", "Gi", "GI", "gi", "gibi", "GIBI", "T", "t", "tera",
        "TERA", "Ti", "TI", "ti", "tebi", "TEBI"}, (new ByteUnits()).getValues());
  }

  /**
   * Test ByteUnits new {@link ByteUnits} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ByteUnits}
   */
  @Test
  public void testByteUnitsNewByteUnits() {
    // Arrange and Act
    ByteUnits actualByteUnits = new ByteUnits();

    // Assert
    assertNull(actualByteUnits.getValue());
    assertEquals(-1, actualByteUnits.getIndex());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SizeSelector#setWhen(SizeComparisons)}
   *   <li>{@link SizeSelector#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    // Act
    sizeSelector.setWhen(new SizeComparisons());

    // Assert
    assertEquals("{sizeselector value: -1 compare: null}", sizeSelector.toString());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code equal}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code equal}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenEqual_whenParameterValueIsEqual() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.WHEN_KEY);
    parameter.setType("Type");
    parameter.setValue("equal");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertNull(sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code K}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code K}.</li>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenK_whenParameterValueIsK_thenSizeSelectorErrorIsNull() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.UNITS_KEY);
    parameter.setType("Type");
    parameter.setValue("K");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertNull(sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Ki}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code Ki}.</li>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenKi_whenParameterValueIsKi_thenSizeSelectorErrorIsNull() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.UNITS_KEY);
    parameter.setType("Type");
    parameter.setValue("Ki");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertNull(sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code M}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code M}.</li>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenM_whenParameterValueIsM_thenSizeSelectorErrorIsNull() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.UNITS_KEY);
    parameter.setType("Type");
    parameter.setValue("M");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertNull(sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Mi}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code Mi}.</li>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenMi_whenParameterValueIsMi_thenSizeSelectorErrorIsNull() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.UNITS_KEY);
    parameter.setType("Type");
    parameter.setValue("Mi");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertNull(sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenName_thenSizeSelectorErrorIsInvalidParameterName() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link SizeSelector} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link SizeSelector} (default constructor) Parameters is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenSizeSelector_whenNull_thenSizeSelectorParametersIsNull() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    // Act
    sizeSelector.setParameters(null);

    // Assert that nothing has changed
    assertNull(sizeSelector.getParameters());
    assertNull(sizeSelector.getError());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link SizeSelector#SIZE_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link SizeSelector#SIZE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenSize_key_whenParameterNameIsSize_key() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.SIZE_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertNull(sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@code Invalid size setting value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenSizeSelectorErrorIsInvalidSizeSettingValue() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(SizeSelector.SIZE_KEY);
    parameter.setType("Type");
    parameter.setValue(SizeSelector.SIZE_KEY);
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid size setting value", sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test {@link SizeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link SizeSelector} (default constructor) Error is {@link SizeSelector#SIZE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenSizeSelectorErrorIsSize_key() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();
    sizeSelector.setError(SizeSelector.SIZE_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    sizeSelector.setParameters(parameters);

    // Assert
    assertEquals(SizeSelector.SIZE_KEY, sizeSelector.getError());
    assertSame(parameters, sizeSelector.getParameters());
  }

  /**
   * Test SizeComparisons new {@link SizeComparisons} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SizeComparisons}
   */
  @Test
  public void testSizeComparisonsNewSizeComparisons() {
    // Arrange and Act
    SizeComparisons actualSizeComparisons = new SizeComparisons();

    // Assert
    assertNull(actualSizeComparisons.getValue());
    assertEquals(-1, actualSizeComparisons.getIndex());
  }

  /**
   * Test {@link SizeSelector#verifySettings()}.
   * <p>
   * Method under test: {@link SizeSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    SizeSelector sizeSelector = new SizeSelector();

    // Act
    sizeSelector.verifySettings();

    // Assert
    assertEquals("The value attribute is required, and must be positive", sizeSelector.getError());
  }

  /**
   * Test {@link SizeSelector#verifySettings()}.
   * <p>
   * Method under test: {@link SizeSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings2() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("The value attribute is required, and must be positive");
    parameter.setType("The value attribute is required, and must be positive");
    parameter.setValue("42");

    SizeSelector sizeSelector = new SizeSelector();
    sizeSelector.setParameters(parameter);

    // Act
    sizeSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter The value attribute is required, and must be positive", sizeSelector.getError());
  }

  /**
   * Test new {@link SizeSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SizeSelector}
   */
  @Test
  public void testNewSizeSelector() {
    // Arrange and Act
    SizeSelector actualSizeSelector = new SizeSelector();

    // Assert
    assertNull(actualSizeSelector.getParameters());
    Location location = actualSizeSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSizeSelector.getDescription());
    assertNull(actualSizeSelector.getError());
    assertNull(actualSizeSelector.getProject());
    assertNull(actualSizeSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSizeSelector.isReference());
  }
}
