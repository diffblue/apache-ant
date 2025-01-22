package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Parameter;
import org.junit.Test;

public class FilenameSelectorDiffblueTest {
  /**
   * Test {@link FilenameSelector#toString()}.
   * <p>
   * Method under test: {@link FilenameSelector#toString()}
   */
  @Test
  public void testToString() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("{filenameselector name: ");

    // Act and Assert
    assertEquals("{filenameselector name: {filenameselector name:  negate: false casesensitive: true}",
        filenameSelector.toString());
  }

  /**
   * Test {@link FilenameSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {filenameselector name: negate: false casesensitive: true}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#toString()}
   */
  @Test
  public void testToString_thenReturnFilenameselectorNameNegateFalseCasesensitiveTrue() {
    // Arrange, Act and Assert
    assertEquals("{filenameselector name:  negate: false casesensitive: true}", (new FilenameSelector()).toString());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link FilenameSelector#CASE_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link FilenameSelector#CASE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenCase_key_whenParameterNameIsCase_key() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(FilenameSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link FilenameSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenName_thenFilenameSelectorErrorIsNull() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertNull(filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link FilenameSelector#NEGATE_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link FilenameSelector#NEGATE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenNegate_key_whenParameterNameIsNegate_key() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(FilenameSelector.NEGATE_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenNull_whenParameterNameIsNull() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(null);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code on}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code on}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenOn_whenParameterValueIsOn() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(FilenameSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("on");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link FilenameSelector#REGEX_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link FilenameSelector#REGEX_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenRegex_key_whenParameterNameIsRegex_key() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(FilenameSelector.REGEX_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link Boolean#TRUE} toString.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@link Boolean#TRUE} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenTrueToString_whenParameterValueIsTrueToString() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(FilenameSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue(Boolean.TRUE.toString());
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code yes}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code yes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenYes_whenParameterValueIsYes() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setError(FilenameSelector.NAME_KEY);

    Parameter parameter = new Parameter();
    parameter.setName(FilenameSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("yes");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals(FilenameSelector.NAME_KEY, filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link FilenameSelector} (default constructor) Error is {@code Invalid parameter null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenFilenameSelectorErrorIsInvalidParameterNull() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();

    Parameter parameter = new Parameter();
    parameter.setName(null);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    filenameSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter null", filenameSelector.getError());
    assertSame(parameters, filenameSelector.getParameters());
  }

  /**
   * Test {@link FilenameSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link FilenameSelector} (default constructor) Parameters is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_whenNull_thenFilenameSelectorParametersIsNull() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();

    // Act
    filenameSelector.setParameters(null);

    // Assert that nothing has changed
    assertNull(filenameSelector.getParameters());
    assertNull(filenameSelector.getError());
  }

  /**
   * Test {@link FilenameSelector#verifySettings()}.
   * <p>
   * Method under test: {@link FilenameSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("The name or regex attribute is required");
    parameter.setType("The name or regex attribute is required");
    parameter.setValue("42");

    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setParameters(parameter);

    // Act
    filenameSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter The name or regex attribute is required", filenameSelector.getError());
  }

  /**
   * Test {@link FilenameSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link FilenameSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenFilenameSelectorErrorIsNull() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("The name or regex attribute is required");

    // Act
    filenameSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(filenameSelector.getError());
  }

  /**
   * Test {@link FilenameSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link FilenameSelector} (default constructor) Error is {@code The name or regex attribute is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenFilenameSelectorErrorIsTheNameOrRegexAttributeIsRequired() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();

    // Act
    filenameSelector.verifySettings();

    // Assert
    assertEquals("The name or regex attribute is required", filenameSelector.getError());
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("The name or regex attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("****");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile3() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("**The name or regex attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link FilenameSelector} (default constructor) Name is {@link SelectorUtils#DEEP_TREE_MATCH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenFilenameSelectorNameIsDeep_tree_match() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName(SelectorUtils.DEEP_TREE_MATCH);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link FilenameSelector} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenFilenameSelectorNameIsEmptyString() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link FilenameSelector} (default constructor) Name is {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenFilenameSelectorNameIsPattern() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("Pattern");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link FilenameSelector} (default constructor) Negate is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenFilenameSelectorNegateIsTrue() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setNegate(true);
    filenameSelector.setName(SelectorUtils.DEEP_TREE_MATCH);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(filenameSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenEmptyString() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("The name or regex attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        filenameSelector.isSelected(basedir, "", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenEmptyString2() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName(SelectorUtils.DEEP_TREE_MATCH);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        filenameSelector.isSelected(basedir, "", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code The name or regex attribute is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenTheNameOrRegexAttributeIsRequired() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("The name or regex attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(filenameSelector.isSelected(basedir, "The name or regex attribute is required",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code **The name or regex attribute is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenTheNameOrRegexAttributeIsRequired2() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("**The name or regex attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(filenameSelector.isSelected(basedir, "**The name or regex attribute is required",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FilenameSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code The name or regex attribute is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilenameSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenTheNameOrRegexAttributeIsRequired3() {
    // Arrange
    FilenameSelector filenameSelector = new FilenameSelector();
    filenameSelector.setName("**The name or regex attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(filenameSelector.isSelected(basedir, "The name or regex attribute is required",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link FilenameSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FilenameSelector}
   */
  @Test
  public void testNewFilenameSelector() {
    // Arrange and Act
    FilenameSelector actualFilenameSelector = new FilenameSelector();

    // Assert
    assertNull(actualFilenameSelector.getParameters());
    Location location = actualFilenameSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFilenameSelector.getDescription());
    assertNull(actualFilenameSelector.getError());
    assertNull(actualFilenameSelector.getProject());
    assertNull(actualFilenameSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualFilenameSelector.isReference());
  }
}
