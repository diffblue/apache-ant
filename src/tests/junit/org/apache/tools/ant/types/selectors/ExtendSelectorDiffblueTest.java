package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class ExtendSelectorDiffblueTest {
  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClassname("There is no classname specified");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector There is no classname specified not initialized, no such class", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate2() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate3() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project()));
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate4() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project()));
    extendSelector.setClassname("ignore");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector ignore not initialized, no such class", extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_givenExtendSelector() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("There is no classname specified", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Classname is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_givenExtendSelectorClassnameIsEmptyString() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClassname("");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("There is no classname specified", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Classpath is {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_givenExtendSelectorClasspathIsPathWithPIsProjectAndPath() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project(), "Path"));
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Classpath is {@link Path#Path(Project)} with project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_givenExtendSelectorClasspathIsPathWithProjectIsNull() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(null));
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Error is {@code There is no classname specified}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_givenExtendSelectorErrorIsThereIsNoClassnameSpecified() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setError("There is no classname specified");

    // Act
    extendSelector.selectorCreate();

    // Assert that nothing has changed
    assertEquals("There is no classname specified", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFilelist(new FileList());

    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(classpath);
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#selectorCreate()}.
   * <ul>
   *   <li>Then {@link ExtendSelector} (default constructor) Error is {@code Selector . not initialized, no such class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#selectorCreate()}
   */
  @Test
  public void testSelectorCreate_thenExtendSelectorErrorIsSelectorNotInitializedNoSuchClass() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project()));
    extendSelector.setClassname(".");

    // Act
    extendSelector.selectorCreate();

    // Assert
    assertEquals("Selector . not initialized, no such class", extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#createClasspath()}.
   * <ul>
   *   <li>Then {@link ExtendSelector} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenExtendSelectorClasspathDescriptionIsNull() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();

    // Act
    Path actualCreateClasspathResult = extendSelector.createClasspath();

    // Assert
    Path classpath = extendSelector.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link ExtendSelector#createClasspath()}.
   * <ul>
   *   <li>Then {@link ExtendSelector} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenExtendSelectorClasspathIsSystemBootClasspath() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = extendSelector.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, extendSelector.getClasspath());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ExtendSelector#setClassname(String)}
   *   <li>{@link ExtendSelector#getClasspath()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();

    // Act
    extendSelector.setClassname("Classname");

    // Assert
    assertNull(extendSelector.getClasspath());
  }

  /**
   * Test {@link ExtendSelector#setClasspathref(Reference)}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor).</li>
   *   <li>Then {@link ExtendSelector} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#setClasspathref(Reference)}
   */
  @Test
  public void testSetClasspathref_givenExtendSelector_thenExtendSelectorClasspathProjectIsNull() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();

    // Act
    extendSelector.setClasspathref(new Reference("42"));

    // Assert
    Path classpath = extendSelector.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClassname("There is no classname specified");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector There is no classname specified not initialized, no such class", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings2() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings3() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project()));
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings4() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project()));
    extendSelector.setClassname("ignore");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector ignore not initialized, no such class", extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenExtendSelector() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("There is no classname specified", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Classname is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenExtendSelectorClassnameIsEmptyString() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setClassname("");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("There is no classname specified", extendSelector.getError());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Classpath is {@link Path#Path(Project, String)} with p is {@code null} and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenExtendSelectorClasspathIsPathWithPIsNullAndPath() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(null, "Path"));
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ExtendSelector} (default constructor) Classpath is {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenExtendSelectorClasspathIsPathWithPIsProjectAndPath() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project(), "Path"));
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFilelist(new FileList());

    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(classpath);
    extendSelector.setClassname("org.apache.tools.ant.types.selectors.FileSelector");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector org.apache.tools.ant.types.selectors.FileSelector not initialized, could not create class",
        extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ExtendSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ExtendSelector} (default constructor) Error is {@code Selector . not initialized, no such class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtendSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenExtendSelectorErrorIsSelectorNotInitializedNoSuchClass() {
    // Arrange
    ExtendSelector extendSelector = new ExtendSelector();
    extendSelector.setProject(new Project());
    extendSelector.setClasspath(new Path(new Project()));
    extendSelector.setClassname(".");

    // Act
    extendSelector.verifySettings();

    // Assert
    assertEquals("Selector . not initialized, no such class", extendSelector.getError());
    assertEquals(1, extendSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test new {@link ExtendSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ExtendSelector}
   */
  @Test
  public void testNewExtendSelector() {
    // Arrange and Act
    ExtendSelector actualExtendSelector = new ExtendSelector();

    // Assert
    Location location = actualExtendSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExtendSelector.getDescription());
    assertNull(actualExtendSelector.getError());
    assertNull(actualExtendSelector.getProject());
    assertNull(actualExtendSelector.getClasspath());
    assertNull(actualExtendSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualExtendSelector.isReference());
  }
}
