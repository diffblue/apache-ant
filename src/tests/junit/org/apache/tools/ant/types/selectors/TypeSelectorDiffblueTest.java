package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Parameter;
import org.apache.tools.ant.types.selectors.TypeSelector.FileType;
import org.junit.Test;

public class TypeSelectorDiffblueTest {
  /**
   * Test FileType {@link FileType#getValues()}.
   * <p>
   * Method under test: {@link FileType#getValues()}
   */
  @Test
  public void testFileTypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{FileType.FILE, FileType.DIR}, (new FileType()).getValues());
  }

  /**
   * Test FileType new {@link FileType} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FileType}
   */
  @Test
  public void testFileTypeNewFileType() {
    // Arrange and Act
    FileType actualFileType = new FileType();

    // Assert
    assertNull(actualFileType.getValue());
    assertEquals(-1, actualFileType.getIndex());
  }

  /**
   * Test new {@link TypeSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TypeSelector}
   */
  @Test
  public void testNewTypeSelector() {
    // Arrange and Act
    TypeSelector actualTypeSelector = new TypeSelector();

    // Assert
    assertNull(actualTypeSelector.getParameters());
    Location location = actualTypeSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTypeSelector.getDescription());
    assertNull(actualTypeSelector.getError());
    assertNull(actualTypeSelector.getProject());
    assertNull(actualTypeSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualTypeSelector.isReference());
  }

  /**
   * Test {@link TypeSelector#toString()}.
   * <p>
   * Method under test: {@link TypeSelector#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("{typeselector type: null}", (new TypeSelector()).toString());
  }

  /**
   * Test {@link TypeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link FileType#FILE}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@link FileType#FILE}.</li>
   *   <li>Then {@link TypeSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenFile_whenParameterValueIsFile_thenTypeSelectorErrorIsNull() {
    // Arrange
    TypeSelector typeSelector = new TypeSelector();

    Parameter parameter = new Parameter();
    parameter.setName(TypeSelector.TYPE_KEY);
    parameter.setType("Type");
    parameter.setValue(FileType.FILE);
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    typeSelector.setParameters(parameters);

    // Assert
    assertNull(typeSelector.getError());
    assertSame(parameters, typeSelector.getParameters());
  }

  /**
   * Test {@link TypeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link TypeSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenName_thenTypeSelectorErrorIsInvalidParameterName() {
    // Arrange
    TypeSelector typeSelector = new TypeSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    typeSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", typeSelector.getError());
    assertSame(parameters, typeSelector.getParameters());
  }

  /**
   * Test {@link TypeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link TypeSelector} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link TypeSelector} (default constructor) Parameters is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenTypeSelector_whenNull_thenTypeSelectorParametersIsNull() {
    // Arrange
    TypeSelector typeSelector = new TypeSelector();

    // Act
    typeSelector.setParameters(null);

    // Assert that nothing has changed
    assertNull(typeSelector.getParameters());
    assertNull(typeSelector.getError());
  }

  /**
   * Test {@link TypeSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link TypeSelector} (default constructor) Error is {@link TypeSelector#TYPE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenTypeSelectorErrorIsType_key() {
    // Arrange
    TypeSelector typeSelector = new TypeSelector();
    typeSelector.setError(TypeSelector.TYPE_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    typeSelector.setParameters(parameters);

    // Assert
    assertEquals(TypeSelector.TYPE_KEY, typeSelector.getError());
    assertSame(parameters, typeSelector.getParameters());
  }

  /**
   * Test {@link TypeSelector#verifySettings()}.
   * <p>
   * Method under test: {@link TypeSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("The type attribute is required");
    parameter.setType("The type attribute is required");
    parameter.setValue("42");

    TypeSelector typeSelector = new TypeSelector();
    typeSelector.setParameters(parameter);

    // Act
    typeSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter The type attribute is required", typeSelector.getError());
  }

  /**
   * Test {@link TypeSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link TypeSelector} (default constructor) Error is {@code The type attribute is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TypeSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenTypeSelectorErrorIsTheTypeAttributeIsRequired() {
    // Arrange
    TypeSelector typeSelector = new TypeSelector();

    // Act
    typeSelector.verifySettings();

    // Assert
    assertEquals("The type attribute is required", typeSelector.getError());
  }
}
