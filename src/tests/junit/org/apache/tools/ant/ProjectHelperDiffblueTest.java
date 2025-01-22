package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.List;
import java.util.Stack;
import java.util.Vector;
import org.apache.tools.ant.ProjectHelper.OnMissingExtensionPoint;
import org.apache.tools.ant.ProjectHelperRepositoryTest.SomeHelper;
import org.apache.tools.ant.helper.ProjectHelper2;
import org.apache.tools.ant.taskdefs.Execute;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class ProjectHelperDiffblueTest {
  /**
   * Test {@link ProjectHelper#getCurrentTargetPrefix()}.
   * <p>
   * Method under test: {@link ProjectHelper#getCurrentTargetPrefix()}
   */
  @Test
  public void testGetCurrentTargetPrefix() {
    // Arrange, Act and Assert
    assertNull(ProjectHelper.getCurrentTargetPrefix());
  }

  /**
   * Test {@link ProjectHelper#getCurrentPrefixSeparator()}.
   * <p>
   * Method under test: {@link ProjectHelper#getCurrentPrefixSeparator()}
   */
  @Test
  public void testGetCurrentPrefixSeparator() {
    // Arrange, Act and Assert
    assertEquals(".", ProjectHelper.getCurrentPrefixSeparator());
  }

  /**
   * Test {@link ProjectHelper#isInIncludeMode()}.
   * <p>
   * Method under test: {@link ProjectHelper#isInIncludeMode()}
   */
  @Test
  public void testIsInIncludeMode() {
    // Arrange, Act and Assert
    assertFalse(ProjectHelper.isInIncludeMode());
  }

  /**
   * Test OnMissingExtensionPoint {@link OnMissingExtensionPoint#valueOf(String)}.
   * <ul>
   *   <li>When {@code fail}.</li>
   *   <li>Then return name is {@code fail}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OnMissingExtensionPoint#valueOf(String)}
   */
  @Test
  public void testOnMissingExtensionPointValueOf_whenFail_thenReturnNameIsFail() {
    // Arrange and Act
    OnMissingExtensionPoint actualValueOfResult = OnMissingExtensionPoint.valueOf("fail");

    // Assert
    assertEquals("fail", actualValueOfResult.name());
    assertEquals("fail", actualValueOfResult.toString());
  }

  /**
   * Test OnMissingExtensionPoint {@link OnMissingExtensionPoint#valueOf(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OnMissingExtensionPoint#valueOf(String)}
   */
  @Test
  public void testOnMissingExtensionPointValueOf_whenName_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> OnMissingExtensionPoint.valueOf("Name"));
  }

  /**
   * Test {@link ProjectHelper#parse(Project, Object)}.
   * <ul>
   *   <li>Given {@link ProjectHelper} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#parse(Project, Object)}
   */
  @Test
  public void testParse_givenProjectHelper_whenProject_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelper projectHelper = new ProjectHelper();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper.parse(new Project(), "Source"));
  }

  /**
   * Test {@link ProjectHelper#getProjectHelper()}.
   * <p>
   * Method under test: {@link ProjectHelper#getProjectHelper()}
   */
  @Test
  public void testGetProjectHelper() {
    // Arrange and Act
    ProjectHelper actualProjectHelper = ProjectHelper.getProjectHelper();

    // Assert
    assertTrue(actualProjectHelper instanceof ProjectHelper2);
    assertTrue(actualProjectHelper.getExtensionStack().isEmpty());
    assertTrue(actualProjectHelper.getImportStack().isEmpty());
    assertEquals(Main.DEFAULT_BUILD_FILENAME, actualProjectHelper.getDefaultBuildFile());
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String, Hashtable)} with {@code project}, {@code value}, {@code keys}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithProjectValueKeys_givenAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42", new Hashtable<>()));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String, Hashtable)} with {@code project}, {@code value}, {@code keys}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithProjectValueKeys_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42", new Hashtable<>()));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String, Hashtable)} with {@code project}, {@code value}, {@code keys}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithProjectValueKeys_givenTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42", new Hashtable<>()));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String, Hashtable)} with {@code project}, {@code value}, {@code keys}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithProjectValueKeys_whenNull() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(null, "42", new Hashtable<>()));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String, Hashtable)} with {@code project}, {@code value}, {@code keys}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithProjectValueKeys_whenProject() throws BuildException {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42", new Hashtable<>()));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String)} with {@code project}, {@code value}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String)}
   */
  @Test
  public void testReplacePropertiesWithProjectValue_givenAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42"));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String)} with {@code project}, {@code value}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String)}
   */
  @Test
  public void testReplacePropertiesWithProjectValue_givenJavaLangObject() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42"));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String)} with {@code project}, {@code value}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String)}
   */
  @Test
  public void testReplacePropertiesWithProjectValue_givenTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(project, "42"));
  }

  /**
   * Test {@link ProjectHelper#replaceProperties(Project, String)} with {@code project}, {@code value}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#replaceProperties(Project, String)}
   */
  @Test
  public void testReplacePropertiesWithProjectValue_whenProject_thenReturn42() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("42", ProjectHelper.replaceProperties(new Project(), "42"));
  }

  /**
   * Test {@link ProjectHelper#parsePropertyString(String, Vector, Vector)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>Then {@link Stack} (default constructor) size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#parsePropertyString(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyString_givenEmptyString_thenStackSizeIsTwo() throws BuildException {
    // Arrange
    Stack<String> fragments = new Stack<>();
    fragments.add("");
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    ProjectHelper.parsePropertyString("42", fragments, propertyRefs);

    // Assert
    assertEquals(2, fragments.size());
    assertEquals("", fragments.get(0));
    assertEquals("42", fragments.get(1));
  }

  /**
   * Test {@link ProjectHelper#parsePropertyString(String, Vector, Vector)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Stack} (default constructor) Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#parsePropertyString(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyString_whenEmptyString_thenStackEmpty() throws BuildException {
    // Arrange
    Stack<String> fragments = new Stack<>();
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    ProjectHelper.parsePropertyString("", fragments, propertyRefs);

    // Assert that nothing has changed
    assertTrue(fragments.isEmpty());
  }

  /**
   * Test {@link ProjectHelper#parsePropertyString(String, Vector, Vector)}.
   * <ul>
   *   <li>When {@link Stack} (default constructor).</li>
   *   <li>Then {@link Stack} (default constructor) size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#parsePropertyString(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyString_whenStack_thenStackSizeIsOne() throws BuildException {
    // Arrange
    Stack<String> fragments = new Stack<>();
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    ProjectHelper.parsePropertyString("42", fragments, propertyRefs);

    // Assert
    assertEquals(1, fragments.size());
    assertEquals("42", fragments.get(0));
  }

  /**
   * Test {@link ProjectHelper#genComponentName(String, String)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#genComponentName(String, String)}
   */
  @Test
  public void testGenComponentName_whenAnt_core_uri_thenReturnName() {
    // Arrange, Act and Assert
    assertEquals("Name", ProjectHelper.genComponentName(ProjectHelper.ANT_CORE_URI, "Name"));
  }

  /**
   * Test {@link ProjectHelper#genComponentName(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#genComponentName(String, String)}
   */
  @Test
  public void testGenComponentName_whenEmptyString_thenReturnName() {
    // Arrange, Act and Assert
    assertEquals("Name", ProjectHelper.genComponentName("", "Name"));
  }

  /**
   * Test {@link ProjectHelper#genComponentName(String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#genComponentName(String, String)}
   */
  @Test
  public void testGenComponentName_whenNull_thenReturnName() {
    // Arrange, Act and Assert
    assertEquals("Name", ProjectHelper.genComponentName(null, "Name"));
  }

  /**
   * Test {@link ProjectHelper#genComponentName(String, String)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   *   <li>Then return {@code Uri:Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#genComponentName(String, String)}
   */
  @Test
  public void testGenComponentName_whenUri_thenReturnUriName() {
    // Arrange, Act and Assert
    assertEquals("Uri:Name", ProjectHelper.genComponentName("Uri", "Name"));
  }

  /**
   * Test {@link ProjectHelper#extractUriFromComponentName(String)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@code antlib}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#extractUriFromComponentName(String)}
   */
  @Test
  public void testExtractUriFromComponentName_whenAnt_core_uri_thenReturnAntlib() {
    // Arrange, Act and Assert
    assertEquals("antlib", ProjectHelper.extractUriFromComponentName(ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ProjectHelper#extractUriFromComponentName(String)}.
   * <ul>
   *   <li>When {@code Component Name}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#extractUriFromComponentName(String)}
   */
  @Test
  public void testExtractUriFromComponentName_whenComponentName_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", ProjectHelper.extractUriFromComponentName("Component Name"));
  }

  /**
   * Test {@link ProjectHelper#extractUriFromComponentName(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#extractUriFromComponentName(String)}
   */
  @Test
  public void testExtractUriFromComponentName_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", ProjectHelper.extractUriFromComponentName(null));
  }

  /**
   * Test {@link ProjectHelper#extractNameFromComponentName(String)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then return {@link MagicNames#ANT_CORE_PACKAGE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#extractNameFromComponentName(String)}
   */
  @Test
  public void testExtractNameFromComponentName_whenAnt_core_uri_thenReturnAnt_core_package() {
    // Arrange, Act and Assert
    assertEquals(MagicNames.ANT_CORE_PACKAGE, ProjectHelper.extractNameFromComponentName(ProjectHelper.ANT_CORE_URI));
  }

  /**
   * Test {@link ProjectHelper#extractNameFromComponentName(String)}.
   * <ul>
   *   <li>When {@code Component Name}.</li>
   *   <li>Then return {@code Component Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#extractNameFromComponentName(String)}
   */
  @Test
  public void testExtractNameFromComponentName_whenComponentName_thenReturnComponentName() {
    // Arrange, Act and Assert
    assertEquals("Component Name", ProjectHelper.extractNameFromComponentName("Component Name"));
  }

  /**
   * Test {@link ProjectHelper#nsToComponentName(String)}.
   * <p>
   * Method under test: {@link ProjectHelper#nsToComponentName(String)}
   */
  @Test
  public void testNsToComponentName() {
    // Arrange, Act and Assert
    assertEquals("attribute namespace:Ns", ProjectHelper.nsToComponentName("Ns"));
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException() {
    // Arrange
    Location newLocation = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper
        .addLocationToBuildException(new ExitStatusException("Msg", 1), newLocation);

    // Assert
    assertTrue(actualAddLocationToBuildExceptionResult instanceof ExitStatusException);
    assertEquals("The following error occurred while executing this line:\nMsg",
        actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertEquals("The following error occurred while executing this line:\nMsg",
        actualAddLocationToBuildExceptionResult.getMessage());
    assertEquals(1, ((ExitStatusException) actualAddLocationToBuildExceptionResult).getStatus());
    Location expectedLocation = newLocation.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualAddLocationToBuildExceptionResult.getLocation());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException2() {
    // Arrange
    BuildException ex = new BuildException("An error occurred");

    // Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper.addLocationToBuildException(ex, null);

    // Assert
    assertEquals("The following error occurred while executing this line:\nAn error occurred",
        actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertEquals("The following error occurred while executing this line:\nAn error occurred",
        actualAddLocationToBuildExceptionResult.getMessage());
    assertSame(ex, actualAddLocationToBuildExceptionResult.getCause());
    assertSame(ex, actualAddLocationToBuildExceptionResult.getException());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException3() {
    // Arrange
    Location newLocation = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper.addLocationToBuildException(
        new ExitStatusException("An error occurred", 1, new Location("foo.txt")), newLocation);

    // Assert
    assertTrue(actualAddLocationToBuildExceptionResult instanceof ExitStatusException);
    assertEquals("The following error occurred while executing this line:\nfoo.txt: An error occurred",
        actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertEquals("The following error occurred while executing this line:\nfoo.txt: An error occurred",
        actualAddLocationToBuildExceptionResult.getMessage());
    assertEquals(1, ((ExitStatusException) actualAddLocationToBuildExceptionResult).getStatus());
    Location expectedLocation = newLocation.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualAddLocationToBuildExceptionResult.getLocation());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException4() {
    // Arrange
    Location newLocation = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper.addLocationToBuildException(
        new ExitStatusException("An error occurred", 1, new Location("foo.txt", 2, 10)), newLocation);

    // Assert
    assertTrue(actualAddLocationToBuildExceptionResult instanceof ExitStatusException);
    assertEquals("The following error occurred while executing this line:\nfoo.txt:2: An error occurred",
        actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertEquals("The following error occurred while executing this line:\nfoo.txt:2: An error occurred",
        actualAddLocationToBuildExceptionResult.getMessage());
    assertEquals(1, ((ExitStatusException) actualAddLocationToBuildExceptionResult).getStatus());
    Location expectedLocation = newLocation.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualAddLocationToBuildExceptionResult.getLocation());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <ul>
   *   <li>Then Cause return {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException_thenCauseReturnBuildException() {
    // Arrange
    Location newLocation = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper
        .addLocationToBuildException(new BuildException("An error occurred"), newLocation);

    // Assert
    Throwable cause = actualAddLocationToBuildExceptionResult.getCause();
    assertTrue(cause instanceof BuildException);
    Location expectedLocation = newLocation.UNKNOWN_LOCATION;
    assertSame(expectedLocation, ((BuildException) cause).getLocation());
    assertSame(cause, actualAddLocationToBuildExceptionResult.getException());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <ul>
   *   <li>Then return LocalizedMessage is {@code An error occurred}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException_thenReturnLocalizedMessageIsAnErrorOccurred() {
    // Arrange and Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper
        .addLocationToBuildException(new ExitStatusException("An error occurred", 1, null), Location.UNKNOWN_LOCATION);

    // Assert
    assertTrue(actualAddLocationToBuildExceptionResult instanceof ExitStatusException);
    assertEquals("An error occurred", actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertEquals("An error occurred", actualAddLocationToBuildExceptionResult.getMessage());
    assertNull(actualAddLocationToBuildExceptionResult.getLocation());
    assertEquals(1, ((ExitStatusException) actualAddLocationToBuildExceptionResult).getStatus());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <ul>
   *   <li>Then return LocalizedMessage is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException_thenReturnLocalizedMessageIsNull() {
    // Arrange
    Location newLocation = Location.UNKNOWN_LOCATION;

    // Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper
        .addLocationToBuildException(new BuildException(), newLocation);

    // Assert
    assertNull(actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertNull(actualAddLocationToBuildExceptionResult.getMessage());
    Location expectedLocation = newLocation.UNKNOWN_LOCATION;
    assertSame(expectedLocation, actualAddLocationToBuildExceptionResult.getLocation());
  }

  /**
   * Test {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#addLocationToBuildException(BuildException, Location)}
   */
  @Test
  public void testAddLocationToBuildException_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    BuildException actualAddLocationToBuildExceptionResult = ProjectHelper
        .addLocationToBuildException(new ExitStatusException("Msg", 1), null);

    // Assert
    assertTrue(actualAddLocationToBuildExceptionResult instanceof ExitStatusException);
    assertEquals("The following error occurred while executing this line:\nMsg",
        actualAddLocationToBuildExceptionResult.getLocalizedMessage());
    assertEquals("The following error occurred while executing this line:\nMsg",
        actualAddLocationToBuildExceptionResult.getMessage());
    Location location = actualAddLocationToBuildExceptionResult.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor2() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setFile(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor3() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor4() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    r.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenDotDot_whenFileResourceNameIsDotDot() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    r.setName("..");

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setName("");

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenEmptyString_whenFileResourceNameIsEmptyString2() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    r.setName("");

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenFileAttributeIsNull() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given {@link ProjectHelper2} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenProjectHelper2_whenResource_thenReturnTrue() {
    // Arrange
    ProjectHelper2 projectHelper2 = new ProjectHelper2();

    // Act and Assert
    assertTrue(projectHelper2.canParseAntlibDescriptor(new Resource()));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given {@link ProjectHelper} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenProjectHelper_whenResource_thenReturnFalse() {
    // Arrange
    ProjectHelper projectHelper = new ProjectHelper();

    // Act and Assert
    assertFalse(projectHelper.canParseAntlibDescriptor(new Resource()));
  }

  /**
   * Test {@link ProjectHelper#canParseAntlibDescriptor(Resource)}.
   * <ul>
   *   <li>Given {@code var}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code var}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseAntlibDescriptor(Resource)}
   */
  @Test
  public void testCanParseAntlibDescriptor_givenVar_whenFileResourceNameIsVar_thenReturnFalse() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource r = new FileResource();
    r.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    r.setName("var");

    // Act and Assert
    assertFalse(someHelper.canParseAntlibDescriptor(r));
  }

  /**
   * Test {@link ProjectHelper#parseAntlibDescriptor(Project, Resource)}.
   * <ul>
   *   <li>Given {@link ProjectHelper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#parseAntlibDescriptor(Project, Resource)}
   */
  @Test
  public void testParseAntlibDescriptor_givenProjectHelper_thenThrowBuildException() {
    // Arrange
    ProjectHelper projectHelper = new ProjectHelper();
    Project containingProject = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelper.parseAntlibDescriptor(containingProject, new Resource()));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    // Act and Assert
    assertFalse(someHelper
        .canParseBuildFile(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile2() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    // Act and Assert
    assertFalse(
        someHelper.canParseBuildFile(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile3() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource buildFile = new FileResource();
    buildFile.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    buildFile.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(buildFile));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenDotDot_whenFileResourceNameIsDotDot_thenReturnFalse() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource buildFile = new FileResource();
    buildFile.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    buildFile.setName("..");

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(buildFile));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource buildFile = new FileResource();
    buildFile.setName("");

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(buildFile));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenEmptyString_whenFileResourceNameIsEmptyString2() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource buildFile = new FileResource();
    buildFile.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    buildFile.setName("");

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(buildFile));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenFileAttributeIsNull() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource buildFile = new FileResource();
    buildFile.setName("file attribute is null!");

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(buildFile));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given {@link ProjectHelper} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenProjectHelper_whenResource_thenReturnTrue() {
    // Arrange
    ProjectHelper projectHelper = new ProjectHelper();

    // Act and Assert
    assertTrue(projectHelper.canParseBuildFile(new Resource()));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given {@link SomeHelper} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenSomeHelper_whenResource_thenReturnFalse() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(new Resource()));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>Given {@code var}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code var}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_givenVar_whenFileResourceNameIsVar_thenReturnFalse() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    FileResource buildFile = new FileResource();
    buildFile.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    buildFile.setName("var");

    // Act and Assert
    assertFalse(someHelper.canParseBuildFile(buildFile));
  }

  /**
   * Test {@link ProjectHelper#canParseBuildFile(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelper#canParseBuildFile(Resource)}
   */
  @Test
  public void testCanParseBuildFile_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange
    SomeHelper someHelper = new SomeHelper();

    // Act and Assert
    assertFalse(
        someHelper.canParseBuildFile(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ProjectHelper}
   *   <li>{@link ProjectHelper#getDefaultBuildFile()}
   *   <li>{@link ProjectHelper#getExtensionStack()}
   *   <li>{@link ProjectHelper#getImportStack()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ProjectHelper actualProjectHelper = new ProjectHelper();
    String actualDefaultBuildFile = actualProjectHelper.getDefaultBuildFile();
    List<String[]> actualExtensionStack = actualProjectHelper.getExtensionStack();
    Vector<Object> actualImportStack = actualProjectHelper.getImportStack();

    // Assert
    assertTrue(actualExtensionStack.isEmpty());
    assertTrue(actualImportStack.isEmpty());
    assertEquals(Main.DEFAULT_BUILD_FILENAME, actualDefaultBuildFile);
  }
}
