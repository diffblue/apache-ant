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
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.JavaResource;
import org.junit.Test;

public class ContainsSelectorDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ContainsSelector#setCasesensitive(boolean)}
   *   <li>{@link ContainsSelector#setEncoding(String)}
   *   <li>{@link ContainsSelector#setIgnorewhitespace(boolean)}
   *   <li>{@link ContainsSelector#setText(String)}
   *   <li>{@link ContainsSelector#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    // Act
    containsSelector.setCasesensitive(true);
    containsSelector.setEncoding("UTF-8");
    containsSelector.setIgnorewhitespace(true);
    containsSelector.setText("Contains");

    // Assert
    assertEquals("{containsselector text: \"Contains\" casesensitive: true ignorewhitespace: true}",
        containsSelector.toString());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link ContainsSelector#CASE_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link ContainsSelector#CASE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenCase_key_whenParameterNameIsCase_key() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertNull(containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link ContainsSelector#CONTAINS_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link ContainsSelector#CONTAINS_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenContains_key_whenParameterNameIsContains_key() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CONTAINS_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertNull(containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link ContainsSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenName_thenContainsSelectorErrorIsInvalidParameterName() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code on}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code on}.</li>
   *   <li>Then {@link ContainsSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenOn_whenParameterValueIsOn_thenContainsSelectorErrorIsNull() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("on");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertNull(containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link Boolean#TRUE} toString.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@link Boolean#TRUE} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenTrueToString_whenParameterValueIsTrueToString() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue(Boolean.TRUE.toString());
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertNull(containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link ContainsSelector#WHITESPACE_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link ContainsSelector#WHITESPACE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenWhitespace_key_whenParameterNameIsWhitespace_key() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.WHITESPACE_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertNull(containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code yes}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code yes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenYes_whenParameterValueIsYes() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("yes");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertNull(containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link ContainsSelector} (default constructor) Error is {@link ContainsSelector#CONTAINS_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenContainsSelectorErrorIsContains_key() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setError(ContainsSelector.CONTAINS_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsSelector.setParameters(parameters);

    // Assert
    assertEquals(ContainsSelector.CONTAINS_KEY, containsSelector.getError());
    assertSame(parameters, containsSelector.getParameters());
  }

  /**
   * Test {@link ContainsSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link ContainsSelector} (default constructor) Parameters is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_whenNull_thenContainsSelectorParametersIsNull() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    // Act
    containsSelector.setParameters(null);

    // Assert that nothing has changed
    assertNull(containsSelector.getParameters());
    assertNull(containsSelector.getError());
  }

  /**
   * Test {@link ContainsSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ContainsSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("The text attribute is required");
    parameter.setType("The text attribute is required");
    parameter.setValue("42");

    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setParameters(parameter);

    // Act
    containsSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter The text attribute is required", containsSelector.getError());
  }

  /**
   * Test {@link ContainsSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ContainsSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenContainsSelectorErrorIsNull() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("foo");

    // Act
    containsSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(containsSelector.getError());
  }

  /**
   * Test {@link ContainsSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ContainsSelector} (default constructor) Error is {@code The text attribute is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenContainsSelectorErrorIsTheTextAttributeIsRequired() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();

    // Act
    containsSelector.verifySettings();

    // Assert
    assertEquals("The text attribute is required", containsSelector.getError());
  }

  /**
   * Test {@link ContainsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The text attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "..", "foo").toFile()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile3() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The text attribute is required", ".").toFile()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile4() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setIgnorewhitespace(true);
    containsSelector.setText("The text attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The text attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link ContainsSelector} (default constructor) Text is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenContainsSelectorTextIsEmptyString() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(containsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The text attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnTrue() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(containsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> containsSelector.isSelected(new JavaResource("The text attribute is required", null)));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@link ContainsSelector} (default constructor) Casesensitive is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_givenContainsSelectorCasesensitiveIsFalse() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("");
    containsSelector.setCasesensitive(false);
    containsSelector.setIgnorewhitespace(false);
    containsSelector.setEncoding(null);

    Resource r = new Resource();
    r.setDirectory(true);

    // Act and Assert
    assertTrue(containsSelector.isSelected(r));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@link ContainsSelector} (default constructor) Text is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_givenContainsSelectorTextIsEmptyString_thenReturnTrue() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("");

    // Act and Assert
    assertTrue(containsSelector.isSelected(new Resource()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");

    FileResource r = new FileResource();
    r.setName("");

    // Act and Assert
    assertTrue(containsSelector.isSelected(r));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_givenFileAttributeIsNull() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsSelector.isSelected(r));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");

    // Act and Assert
    assertTrue(containsSelector
        .isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_whenJavaConstantResource_thenThrowBuildException() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsSelector.isSelected(new JavaConstantResource()));
  }

  /**
   * Test {@link ContainsSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link JavaResource#JavaResource(String, Path)} with name is empty string and path is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_whenJavaResourceWithNameIsEmptyStringAndPathIsNull() {
    // Arrange
    ContainsSelector containsSelector = new ContainsSelector();
    containsSelector.setText("The text attribute is required");

    // Act and Assert
    assertFalse(containsSelector.isSelected(new JavaResource("", null)));
  }

  /**
   * Test new {@link ContainsSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ContainsSelector}
   */
  @Test
  public void testNewContainsSelector() {
    // Arrange and Act
    ContainsSelector actualContainsSelector = new ContainsSelector();

    // Assert
    assertNull(actualContainsSelector.getParameters());
    Location location = actualContainsSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualContainsSelector.getDescription());
    assertNull(actualContainsSelector.getError());
    assertNull(actualContainsSelector.getProject());
    assertNull(actualContainsSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualContainsSelector.isReference());
  }
}
