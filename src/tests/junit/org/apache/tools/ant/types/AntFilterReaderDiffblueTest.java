package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class AntFilterReaderDiffblueTest {
  /**
   * Test {@link AntFilterReader#setClassName(String)}.
   * <p>
   * Method under test: {@link AntFilterReader#setClassName(String)}
   */
  @Test
  public void testSetClassName() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();

    // Act
    antFilterReader.setClassName("Class Name");

    // Assert
    assertEquals("Class Name", antFilterReader.getClassName());
  }

  /**
   * Test {@link AntFilterReader#getClassName()}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#getClassName()}
   */
  @Test
  public void testGetClassName_givenAntFilterReader_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new AntFilterReader()).getClassName());
  }

  /**
   * Test {@link AntFilterReader#addParam(Parameter)}.
   * <p>
   * Method under test: {@link AntFilterReader#addParam(Parameter)}
   */
  @Test
  public void testAddParam() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();

    Parameter param = new Parameter();
    param.setName("Name");
    param.setType("Type");
    param.setValue("42");

    // Act
    antFilterReader.addParam(param);

    // Assert
    Parameter[] params = antFilterReader.getParams();
    assertEquals(1, params.length);
    assertSame(param, params[0]);
  }

  /**
   * Test {@link AntFilterReader#setClasspath(Path)}.
   * <p>
   * Method under test: {@link AntFilterReader#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath() {
    // Arrange
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(classpath);
    Path classpath2 = new Path(null);

    // Act
    antFilterReader.setClasspath(classpath2);

    // Assert that nothing has changed
    assertFalse(antFilterReader.isChecked());
    Path expectedClasspath = classpath2.systemBootClasspath;
    assertSame(expectedClasspath, antFilterReader.getClasspath());
  }

  /**
   * Test {@link AntFilterReader#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor) Classpath is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenAntFilterReaderClasspathIsPathWithProjectIsProject() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(new Path(new Project()));

    // Act
    antFilterReader.setClasspath(Path.systemBootClasspath);

    // Assert
    assertFalse(antFilterReader.getClasspath().isChecked());
  }

  /**
   * Test {@link AntFilterReader#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link AntFilterReader} (default constructor) Classpath Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenNotAntFilterReaderClasspathChecked() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(Path.systemBootClasspath);

    // Act
    antFilterReader.setClasspath(null);

    // Assert that nothing has changed
    assertFalse(antFilterReader.isChecked());
    assertFalse(antFilterReader.getClasspath().isChecked());
  }

  /**
   * Test {@link AntFilterReader#createClasspath()}.
   * <ul>
   *   <li>Then {@link AntFilterReader} (default constructor) Classpath DataTypeName is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenAntFilterReaderClasspathDataTypeNameIsPath() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();

    // Act
    Path actualCreateClasspathResult = antFilterReader.createClasspath();

    // Assert
    Path classpath = antFilterReader.getClasspath();
    assertEquals("Path", classpath.getDataTypeName());
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(antFilterReader.isChecked());
    assertFalse(classpath.isChecked());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link AntFilterReader#createClasspath()}.
   * <ul>
   *   <li>Then {@link AntFilterReader} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenAntFilterReaderClasspathIsSystemBootClasspath() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(Path.systemBootClasspath);

    // Act
    Path actualCreateClasspathResult = antFilterReader.createClasspath();

    // Assert
    assertFalse(antFilterReader.isChecked());
    Path expectedClasspath = actualCreateClasspathResult.systemBootClasspath;
    assertSame(expectedClasspath, antFilterReader.getClasspath());
  }

  /**
   * Test {@link AntFilterReader#createClasspath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnProjectIsProject() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    Project project = new Project();
    antFilterReader.setProject(project);

    // Act and Assert
    assertSame(project, antFilterReader.createClasspath().getProject());
  }

  /**
   * Test {@link AntFilterReader#getClasspath()}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenAntFilterReader_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new AntFilterReader()).getClasspath());
  }

  /**
   * Test {@link AntFilterReader#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnSystemBootClasspath() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(Path.systemBootClasspath);

    // Act
    Path actualClasspath = antFilterReader.getClasspath();

    // Assert
    assertSame(actualClasspath.systemBootClasspath, actualClasspath);
  }

  /**
   * Test {@link AntFilterReader#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link AntFilterReader} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenAntFilterReaderClasspathProjectIsNull() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();

    // Act
    antFilterReader.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = antFilterReader.getClasspath();
    assertEquals("Path", classpath.getDataTypeName());
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(antFilterReader.isChecked());
    assertFalse(classpath.isChecked());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link AntFilterReader#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link AntFilterReader} (default constructor) Classpath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenAntFilterReaderClasspathProjectIsProject() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    Project project = new Project();
    antFilterReader.setProject(project);

    // Act
    antFilterReader.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = antFilterReader.getClasspath();
    assertEquals("Path", classpath.getDataTypeName());
    assertNull(classpath.getDescription());
    assertNull(classpath.getRefid());
    assertFalse(antFilterReader.isChecked());
    assertFalse(classpath.isChecked());
    assertFalse(classpath.isReference());
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link AntFilterReader#getParams()}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#getParams()}
   */
  @Test
  public void testGetParams_givenAntFilterReader() {
    // Arrange, Act and Assert
    assertEquals(0, (new AntFilterReader()).getParams().length);
  }

  /**
   * Test {@link AntFilterReader#getParams()}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#getParams()}
   */
  @Test
  public void testGetParams_givenAntFilterReaderClasspathIsSystemBootClasspath() {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    assertEquals(0, antFilterReader.getParams().length);
  }

  /**
   * Test {@link AntFilterReader#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor).</li>
   *   <li>Then not {@link AntFilterReader} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenAntFilterReader_thenNotAntFilterReaderChecked() throws BuildException {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    Reference r = new Reference("42");

    // Act
    antFilterReader.setRefid(r);

    // Assert
    assertFalse(antFilterReader.isChecked());
    assertTrue(antFilterReader.isReference());
    assertSame(r, antFilterReader.getRefid());
  }

  /**
   * Test {@link AntFilterReader#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42() throws BuildException {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();

    Stack<Object> stk = new Stack<>();
    stk.add("42");

    // Act
    antFilterReader.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(antFilterReader.isChecked());
  }

  /**
   * Test {@link AntFilterReader#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd422() throws BuildException {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();

    Stack<Object> stk = new Stack<>();
    stk.add("42");
    stk.add("42");

    // Act
    antFilterReader.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(antFilterReader.isChecked());
  }

  /**
   * Test {@link AntFilterReader#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor) Classpath is {@code null}.</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenAntFilterReaderClasspathIsNull_whenStack() throws BuildException {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    antFilterReader.setClasspath(null);
    Stack<Object> stk = new Stack<>();

    // Act
    antFilterReader.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(antFilterReader.isChecked());
  }

  /**
   * Test {@link AntFilterReader#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link AntFilterReader} (default constructor).</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntFilterReader#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenAntFilterReader_whenStack() throws BuildException {
    // Arrange
    AntFilterReader antFilterReader = new AntFilterReader();
    Stack<Object> stk = new Stack<>();

    // Act
    antFilterReader.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(antFilterReader.isChecked());
  }

  /**
   * Test new {@link AntFilterReader} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AntFilterReader}
   */
  @Test
  public void testNewAntFilterReader() {
    // Arrange and Act
    AntFilterReader actualAntFilterReader = new AntFilterReader();

    // Assert
    Location location = actualAntFilterReader.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAntFilterReader.getDescription());
    assertNull(actualAntFilterReader.getProject());
    assertNull(actualAntFilterReader.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualAntFilterReader.isChecked());
  }
}
