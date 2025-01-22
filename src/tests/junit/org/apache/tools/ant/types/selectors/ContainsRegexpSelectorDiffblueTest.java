package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Parameter;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.JavaResource;
import org.junit.Test;

public class ContainsRegexpSelectorDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ContainsRegexpSelector#setCaseSensitive(boolean)}
   *   <li>{@link ContainsRegexpSelector#setExpression(String)}
   *   <li>{@link ContainsRegexpSelector#setMultiLine(boolean)}
   *   <li>{@link ContainsRegexpSelector#setSingleLine(boolean)}
   *   <li>{@link ContainsRegexpSelector#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    // Act
    containsRegexpSelector.setCaseSensitive(true);
    containsRegexpSelector.setExpression("Theexpression");
    containsRegexpSelector.setMultiLine(true);
    containsRegexpSelector.setSingleLine(true);

    // Assert
    assertEquals("{containsregexpselector expression: Theexpression}", containsRegexpSelector.toString());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link ContainsSelector#CASE_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link ContainsSelector#CASE_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenCase_key_whenParameterNameIsCase_key() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link ContainsRegexpSelector#EXPRESSION_KEY}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@link ContainsRegexpSelector#EXPRESSION_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenExpression_key_whenParameterNameIsExpression_key() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsRegexpSelector.EXPRESSION_KEY);
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code multiline}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@code multiline}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenMultiline_whenParameterNameIsMultiline() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName("multiline");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code on}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code on}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenOn_whenParameterValueIsOn() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("on");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code singleline}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@code singleline}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenSingleline_whenParameterNameIsSingleline() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName("singleline");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@link Boolean#TRUE} toString.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@link Boolean#TRUE} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenTrueToString_whenParameterValueIsTrueToString() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue(Boolean.TRUE.toString());
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Given {@code yes}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code yes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_givenYes_whenParameterValueIsYes() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName(ContainsSelector.CASE_KEY);
    parameter.setType("Type");
    parameter.setValue("yes");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertNull(containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link ContainsRegexpSelector} (default constructor) Error is {@link ContainsRegexpSelector#EXPRESSION_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenContainsRegexpSelectorErrorIsExpression_key() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setError(ContainsRegexpSelector.EXPRESSION_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertEquals(ContainsRegexpSelector.EXPRESSION_KEY, containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link ContainsRegexpSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenContainsRegexpSelectorErrorIsInvalidParameterName() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link ContainsRegexpSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link ContainsRegexpSelector} (default constructor) Parameters is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_whenNull_thenContainsRegexpSelectorParametersIsNull() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    // Act
    containsRegexpSelector.setParameters(null);

    // Assert that nothing has changed
    assertNull(containsRegexpSelector.getParameters());
    assertNull(containsRegexpSelector.getError());
  }

  /**
   * Test {@link ContainsRegexpSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    // Act
    containsRegexpSelector.verifySettings();

    // Assert
    assertEquals("The expression attribute is required", containsRegexpSelector.getError());
  }

  /**
   * Test {@link ContainsRegexpSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings2() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("The expression attribute is required");
    parameter.setType("The expression attribute is required");
    parameter.setValue("42");

    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setParameters(parameter);

    // Act
    containsRegexpSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Invalid parameter The expression attribute is required", containsRegexpSelector.getError());
  }

  /**
   * Test {@link ContainsRegexpSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ContainsRegexpSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenContainsRegexpSelectorErrorIsNull() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("foo");

    // Act
    containsRegexpSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(containsRegexpSelector.getError());
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The expression attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setProject(new Project());
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The expression attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile3() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "..", "foo").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile4() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The expression attribute is required", ".").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile5() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setProject(project);
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The expression attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setProject(project);
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The expression attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setProject(project);
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "The expression attribute is required", "foo").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnTrue() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(containsRegexpSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(Resource)} with {@code r}.
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> containsRegexpSelector.isSelected(new JavaResource("The expression attribute is required", null)));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_givenTrue_whenResourceDirectoryIsTrue_thenReturnTrue() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");

    Resource r = new Resource();
    r.setDirectory(true);

    // Act and Assert
    assertTrue(containsRegexpSelector.isSelected(r));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");

    // Act and Assert
    assertTrue(containsRegexpSelector
        .isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link ContainsRegexpSelector#isSelected(Resource)} with {@code r}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContainsRegexpSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithR_whenJavaConstantResource_thenThrowBuildException() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setExpression("The expression attribute is required");

    // Act and Assert
    assertThrows(BuildException.class, () -> containsRegexpSelector.isSelected(new JavaConstantResource()));
  }

  /**
   * Test new {@link ContainsRegexpSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ContainsRegexpSelector}
   */
  @Test
  public void testNewContainsRegexpSelector() {
    // Arrange and Act
    ContainsRegexpSelector actualContainsRegexpSelector = new ContainsRegexpSelector();

    // Assert
    assertNull(actualContainsRegexpSelector.getParameters());
    Location location = actualContainsRegexpSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualContainsRegexpSelector.getDescription());
    assertNull(actualContainsRegexpSelector.getError());
    assertNull(actualContainsRegexpSelector.getProject());
    assertNull(actualContainsRegexpSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualContainsRegexpSelector.isReference());
  }
}
