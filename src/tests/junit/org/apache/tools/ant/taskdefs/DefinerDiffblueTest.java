package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Definer.Format;
import org.apache.tools.ant.taskdefs.Definer.OnError;
import org.junit.Test;

public class DefinerDiffblueTest {
  /**
   * Test Format {@link Format#getValues()}.
   * <p>
   * Method under test: {@link Format#getValues()}
   */
  @Test
  public void testFormatGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"properties", "xml"}, (new Format()).getValues());
  }

  /**
   * Test Format new {@link Format} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Format}
   */
  @Test
  public void testFormatNewFormat() {
    // Arrange and Act
    Format actualFormat = new Format();

    // Assert
    assertNull(actualFormat.getValue());
    assertEquals(-1, actualFormat.getIndex());
  }

  /**
   * Test {@link Definer#getName()}.
   * <p>
   * Method under test: {@link Definer#getName()}
   */
  @Test
  public void testGetName() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getName());
  }

  /**
   * Test {@link Definer#getFile()}.
   * <p>
   * Method under test: {@link Definer#getFile()}
   */
  @Test
  public void testGetFile() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getFile());
  }

  /**
   * Test {@link Definer#getResource()}.
   * <p>
   * Method under test: {@link Definer#getResource()}
   */
  @Test
  public void testGetResource() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getResource());
  }

  /**
   * Test {@link Definer#execute()}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) AntlibClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#execute()}
   */
  @Test
  public void testExecute_givenComponentdefAntlibClassLoaderIsAntClassLoader() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setAntlibClassLoader(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.execute());
  }

  /**
   * Test {@link Definer#execute()}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#execute()}
   */
  @Test
  public void testExecute_givenComponentdefProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.execute());
  }

  /**
   * Test {@link Definer#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.execute());
  }

  /**
   * Test {@link Definer#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.execute());
  }

  /**
   * Test {@link Definer#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#execute()}
   */
  @Test
  public void testExecute_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.execute());
  }

  /**
   * Test {@link Definer#makeResourceFromURI(String)}.
   * <ul>
   *   <li>When {@code antlib://.xml}.</li>
   *   <li>Then return {@code .xml}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#makeResourceFromURI(String)}
   */
  @Test
  public void testMakeResourceFromURI_whenAntlibXml_thenReturnXml() {
    // Arrange, Act and Assert
    assertEquals(".xml", Definer.makeResourceFromURI("antlib://.xml"));
  }

  /**
   * Test {@link Definer#makeResourceFromURI(String)}.
   * <ul>
   *   <li>When {@code antlib:}.</li>
   *   <li>Then return {@code /antlib.xml}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#makeResourceFromURI(String)}
   */
  @Test
  public void testMakeResourceFromURI_whenAntlib_thenReturnAntlibXml() {
    // Arrange, Act and Assert
    assertEquals("/antlib.xml", Definer.makeResourceFromURI("antlib:"));
  }

  /**
   * Test {@link Definer#makeResourceFromURI(String)}.
   * <ul>
   *   <li>When {@code antlib://}.</li>
   *   <li>Then return {@code /antlib.xml}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#makeResourceFromURI(String)}
   */
  @Test
  public void testMakeResourceFromURI_whenAntlib_thenReturnAntlibXml2() {
    // Arrange, Act and Assert
    assertEquals("/antlib.xml", Definer.makeResourceFromURI("antlib://"));
  }

  /**
   * Test {@link Definer#loadProperties(ClassLoader, URL)}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) URI is {@code class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#loadProperties(ClassLoader, URL)}
   */
  @Test
  public void testLoadProperties_givenComponentdefUriIsClass() throws MalformedURLException, BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setURI(" class ");

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.loadProperties(new AntClassLoader(),
        Paths.get(System.getProperty("java.io.tmpdir"), "foo").toUri().toURL()));
  }

  /**
   * Test {@link Definer#loadProperties(ClassLoader, URL)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code class} toUri toURL.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#loadProperties(ClassLoader, URL)}
   */
  @Test
  public void testLoadProperties_whenPropertyIsJavaIoTmpdirIsClassToUriToURL() throws MalformedURLException {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.loadProperties(new AntClassLoader(),
        Paths.get(System.getProperty("java.io.tmpdir"), " class ").toUri().toURL()));
  }

  /**
   * Test {@link Definer#loadProperties(ClassLoader, URL)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toUri toURL.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#loadProperties(ClassLoader, URL)}
   */
  @Test
  public void testLoadProperties_whenPropertyIsJavaIoTmpdirIsFooToUriToURL() throws MalformedURLException {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.loadProperties(new AntClassLoader(),
        Paths.get(System.getProperty("java.io.tmpdir"), "foo").toUri().toURL()));
  }

  /**
   * Test OnError {@link OnError#getValues()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@link OnError#POLICY_FAIL} and {@link OnError#POLICY_REPORT}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OnError#getValues()}
   */
  @Test
  public void testOnErrorGetValues_thenReturnArrayOfStringWithPolicy_failAndPolicy_report() {
    // Arrange, Act and Assert
    assertArrayEquals(
        new String[]{OnError.POLICY_FAIL, OnError.POLICY_REPORT, OnError.POLICY_IGNORE, OnError.POLICY_FAILALL},
        (new OnError()).getValues());
  }

  /**
   * Test OnError {@link OnError#OnError()}.
   * <p>
   * Method under test: {@link OnError#OnError()}
   */
  @Test
  public void testOnErrorNewOnError() {
    // Arrange and Act
    OnError actualOnError = new OnError();

    // Assert
    assertNull(actualOnError.getValue());
    assertEquals(-1, actualOnError.getIndex());
  }

  /**
   * Test OnError {@link OnError#OnError(String)}.
   * <ul>
   *   <li>When {@link OnError#POLICY_FAIL}.</li>
   *   <li>Then return Index is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link OnError#OnError(String)}
   */
  @Test
  public void testOnErrorNewOnError_whenPolicy_fail_thenReturnIndexIsZero() {
    // Arrange and Act
    OnError actualOnError = new OnError(OnError.POLICY_FAIL);

    // Assert
    assertEquals(0, actualOnError.getIndex());
    assertEquals(OnError.POLICY_FAIL, actualOnError.getValue());
    assertArrayEquals(
        new String[]{OnError.POLICY_FAIL, OnError.POLICY_REPORT, OnError.POLICY_IGNORE, OnError.POLICY_FAILALL},
        actualOnError.getValues());
  }

  /**
   * Test {@link Definer#setFile(File)}.
   * <p>
   * Method under test: {@link Definer#setFile(File)}
   */
  @Test
  public void testSetFile() {
    // Arrange
    Componentdef componentdef = new Componentdef();
    File file = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    componentdef.setFile(file);

    // Assert
    assertSame(file, componentdef.getFile());
  }

  /**
   * Test {@link Definer#setResource(String)}.
   * <p>
   * Method under test: {@link Definer#setResource(String)}
   */
  @Test
  public void testSetResource() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setResource("Res");

    // Assert
    assertEquals("Res", componentdef.getResource());
  }

  /**
   * Test {@link Definer#setAntlib(String)}.
   * <ul>
   *   <li>When {@code antlib:}.</li>
   *   <li>Then {@link Componentdef} (default constructor) Resource is {@code /antlib.xml}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#setAntlib(String)}
   */
  @Test
  public void testSetAntlib_whenAntlib_thenComponentdefResourceIsAntlibXml() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setAntlib("antlib:");

    // Assert
    assertEquals("/antlib.xml", componentdef.getResource());
    assertEquals("antlib:", componentdef.getURI());
  }

  /**
   * Test {@link Definer#setAntlib(String)}.
   * <ul>
   *   <li>When {@code Antlib}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#setAntlib(String)}
   */
  @Test
  public void testSetAntlib_whenAntlib_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Componentdef()).setAntlib("Antlib"));
  }

  /**
   * Test {@link Definer#setName(String)}.
   * <p>
   * Method under test: {@link Definer#setName(String)}
   */
  @Test
  public void testSetName() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals(Manifest.ATTRIBUTE_NAME, componentdef.getName());
  }

  /**
   * Test {@link Definer#getClassname()}.
   * <p>
   * Method under test: {@link Definer#getClassname()}
   */
  @Test
  public void testGetClassname() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getClassname());
  }

  /**
   * Test {@link Definer#setClassname(String)}.
   * <p>
   * Method under test: {@link Definer#setClassname(String)}
   */
  @Test
  public void testSetClassname() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setClassname("Classname");

    // Assert
    assertEquals("Classname", componentdef.getClassname());
  }

  /**
   * Test {@link Definer#addDefinition(ClassLoader, String, String)}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) AdaptTo is {@code alice.liddell@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#addDefinition(ClassLoader, String, String)}
   */
  @Test
  public void testAddDefinition_givenComponentdefAdaptToIsAliceLiddellExampleOrg() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setAdaptTo("alice.liddell@example.org");
    componentdef.setURI("foo");
    componentdef.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> componentdef.addDefinition(new AntClassLoader(), Manifest.ATTRIBUTE_NAME, "java.lang.String"));
  }

  /**
   * Test {@link Definer#addDefinition(ClassLoader, String, String)}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) Adapter is {@code antlib:org.apache.tools.ant}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#addDefinition(ClassLoader, String, String)}
   */
  @Test
  public void testAddDefinition_givenComponentdefAdapterIsAntlibOrgApacheToolsAnt() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setAdapter("antlib:org.apache.tools.ant");
    componentdef.setURI("foo");
    componentdef.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> componentdef.addDefinition(new AntClassLoader(), Manifest.ATTRIBUTE_NAME, "java.lang.String"));
  }

  /**
   * Test {@link Definer#addDefinition(ClassLoader, String, String)}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor).</li>
   *   <li>When {@code Classname}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#addDefinition(ClassLoader, String, String)}
   */
  @Test
  public void testAddDefinition_givenComponentdef_whenClassname_thenThrowBuildException() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> componentdef.addDefinition(new AntClassLoader(), Manifest.ATTRIBUTE_NAME, "Classname"));
  }

  /**
   * Test {@link Definer#addDefinition(ClassLoader, String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addDataTypeDefinition {@code Finding class} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#addDefinition(ClassLoader, String, String)}
   */
  @Test
  public void testAddDefinition_givenProjectAddDataTypeDefinitionFindingClassAndObject() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Finding class ", typeClass);

    AntClassLoader al = new AntClassLoader();
    al.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.addDefinition(al, Manifest.ATTRIBUTE_NAME, "Classname"));
  }

  /**
   * Test {@link Definer#addDefinition(ClassLoader, String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#addDefinition(ClassLoader, String, String)}
   */
  @Test
  public void testAddDefinition_givenProject_whenAntClassLoaderProjectIsProject() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();

    AntClassLoader al = new AntClassLoader();
    al.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> componentdef.addDefinition(al, Manifest.ATTRIBUTE_NAME, "Classname"));
  }

  /**
   * Test {@link Definer#addDefinition(ClassLoader, String, String)}.
   * <ul>
   *   <li>When {@code Classname}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Definer#addDefinition(ClassLoader, String, String)}
   */
  @Test
  public void testAddDefinition_whenClassname_thenThrowBuildException() throws BuildException {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setURI("foo");
    componentdef.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> componentdef.addDefinition(new AntClassLoader(), Manifest.ATTRIBUTE_NAME, "Classname"));
  }
}
