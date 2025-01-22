package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Parameter;
import org.junit.Test;

public class DepthSelectorDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DepthSelector#setMax(int)}
   *   <li>{@link DepthSelector#setMin(int)}
   *   <li>{@link DepthSelector#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    // Act
    depthSelector.setMax(3);
    depthSelector.setMin(1);

    // Assert
    assertEquals("{depthselector min: 1 max: 3}", depthSelector.toString());
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link DepthSelector} (default constructor) {@link DepthSelector#min} is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenDepthSelector_whenNull_thenDepthSelectorMinIsMinusOne() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    // Act
    depthSelector.setParameters(null);

    // Assert that nothing has changed
    assertEquals(-1, depthSelector.max);
    assertEquals(-1, depthSelector.min);
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DepthSelector#MAX_KEY}.</li>
   *   <li>Then {@link DepthSelector} (default constructor) {@link DepthSelector#max} is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenMax_key_thenDepthSelectorMaxIsFortyTwo() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DepthSelector.MAX_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    depthSelector.setParameters(parameters);

    // Assert
    assertNull(depthSelector.getError());
    assertEquals(-1, depthSelector.min);
    assertEquals(42, depthSelector.max);
    assertSame(parameters, depthSelector.getParameters());
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link DepthSelector#MIN_KEY}.</li>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenMin_key_thenDepthSelectorErrorIsNull() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DepthSelector.MIN_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    depthSelector.setParameters(parameters);

    // Assert
    assertNull(depthSelector.getError());
    assertEquals(-1, depthSelector.max);
    assertEquals(42, depthSelector.min);
    assertSame(parameters, depthSelector.getParameters());
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenName_thenDepthSelectorErrorIsInvalidParameterName() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    depthSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", depthSelector.getError());
    assertEquals(-1, depthSelector.max);
    assertEquals(-1, depthSelector.min);
    assertSame(parameters, depthSelector.getParameters());
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code Invalid maximum value min}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenDepthSelectorErrorIsInvalidMaximumValueMin() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DepthSelector.MIN_KEY);
    parameter.setType(DepthSelector.MIN_KEY);
    parameter.setValue("42");

    Parameter parameter2 = new Parameter();
    parameter2.setName(DepthSelector.MAX_KEY);
    parameter2.setType(DepthSelector.MAX_KEY);
    parameter2.setValue(DepthSelector.MIN_KEY);
    Parameter[] parameters = new Parameter[]{parameter, parameter2};

    // Act
    depthSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid maximum value min", depthSelector.getError());
    assertEquals(-1, depthSelector.max);
    assertEquals(42, depthSelector.min);
    assertSame(parameters, depthSelector.getParameters());
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code Invalid minimum value min}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenDepthSelectorErrorIsInvalidMinimumValueMin() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    Parameter parameter = new Parameter();
    parameter.setName(DepthSelector.MIN_KEY);
    parameter.setType("Type");
    parameter.setValue(DepthSelector.MIN_KEY);
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    depthSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid minimum value min", depthSelector.getError());
    assertEquals(-1, depthSelector.max);
    assertEquals(-1, depthSelector.min);
    assertSame(parameters, depthSelector.getParameters());
  }

  /**
   * Test {@link DepthSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@link DepthSelector#MIN_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenDepthSelectorErrorIsMin_key() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setError(DepthSelector.MIN_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    depthSelector.setParameters(parameters);

    // Assert
    assertEquals(-1, depthSelector.max);
    assertEquals(-1, depthSelector.min);
    assertEquals(DepthSelector.MIN_KEY, depthSelector.getError());
    assertSame(parameters, depthSelector.getParameters());
  }

  /**
   * Test {@link DepthSelector#verifySettings()}.
   * <p>
   * Method under test: {@link DepthSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();

    // Act
    depthSelector.verifySettings();

    // Assert
    assertEquals("You must set at least one of the min or the max levels.", depthSelector.getError());
  }

  /**
   * Test {@link DepthSelector#verifySettings()}.
   * <p>
   * Method under test: {@link DepthSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings2() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("You must set at least one of the min or the max levels.");
    parameter.setType("You must set at least one of the min or the max levels.");
    parameter.setValue("42");

    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setParameters(parameter);

    // Act
    depthSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter You must set at least one of the min or the max levels.", depthSelector.getError());
  }

  /**
   * Test {@link DepthSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Max is minus one.</li>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenDepthSelectorMaxIsMinusOne_thenDepthSelectorErrorIsNull() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(0);
    depthSelector.setMax(-1);

    // Act
    depthSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(depthSelector.getError());
  }

  /**
   * Test {@link DepthSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Min is minus one.</li>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenDepthSelectorMinIsMinusOne_thenDepthSelectorErrorIsNull() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(-1);
    depthSelector.setMax(0);

    // Act
    depthSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(depthSelector.getError());
  }

  /**
   * Test {@link DepthSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Min is zero.</li>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenDepthSelectorMinIsZero_thenDepthSelectorErrorIsNull() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(0);
    depthSelector.setMax(0);

    // Act
    depthSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(depthSelector.getError());
  }

  /**
   * Test {@link DepthSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link DepthSelector} (default constructor) Error is {@code The maximum depth is lower than the minimum.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenDepthSelectorErrorIsTheMaximumDepthIsLowerThanTheMinimum() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(1);
    depthSelector.setMax(0);

    // Act
    depthSelector.verifySettings();

    // Assert
    assertEquals("The maximum depth is lower than the minimum.", depthSelector.getError());
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Max is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenDepthSelectorMaxIsThree() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMax(3);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Max is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenDepthSelectorMaxIsThree2() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMax(3);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertTrue(depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Min is one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenDepthSelectorMinIsOne_thenReturnFalse() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(1);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Min is one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenDepthSelectorMinIsOne_thenReturnFalse2() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(1);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link DepthSelector} (default constructor) Min is zero.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenDepthSelectorMinIsZero_thenReturnTrue() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(0);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertTrue(depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenThrowBuildException() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(1);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()));
  }

  /**
   * Test {@link DepthSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code var} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DepthSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenPropertyIsJavaIoTmpdirIsVarToFile() {
    // Arrange
    DepthSelector depthSelector = new DepthSelector();
    depthSelector.setMin(1);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> depthSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link DepthSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DepthSelector}
   */
  @Test
  public void testNewDepthSelector() {
    // Arrange and Act
    DepthSelector actualDepthSelector = new DepthSelector();

    // Assert
    assertNull(actualDepthSelector.getParameters());
    Location location = actualDepthSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDepthSelector.getDescription());
    assertNull(actualDepthSelector.getError());
    assertNull(actualDepthSelector.getProject());
    assertNull(actualDepthSelector.getRefid());
    assertEquals(-1, actualDepthSelector.max);
    assertEquals(-1, actualDepthSelector.min);
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualDepthSelector.isReference());
  }
}
