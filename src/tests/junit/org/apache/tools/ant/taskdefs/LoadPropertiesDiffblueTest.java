package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.filters.ClassConstants;
import org.apache.tools.ant.filters.HeadFilter;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.types.AntFilterReader;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FilterChain;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class LoadPropertiesDiffblueTest {
  /**
   * Test {@link LoadProperties#setSrcFile(File)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#setSrcFile(File)}
   */
  @Test
  public void testSetSrcFile_thenThrowBuildException() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.setSrcFile(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link LoadProperties#setResource(String)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#setResource(String)}
   */
  @Test
  public void testSetResource_thenThrowBuildException() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.setResource("Resource"));
  }

  /**
   * Test {@link LoadProperties#setClasspath(Path)}.
   * <p>
   * Method under test: {@link LoadProperties#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    Path classpath = Path.systemBootClasspath;

    // Act
    loadProperties.setClasspath(classpath);

    // Assert
    Path expectedClasspath = classpath.systemBootClasspath;
    assertSame(expectedClasspath, loadProperties.getClasspath());
  }

  /**
   * Test {@link LoadProperties#setClasspath(Path)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenThrowBuildException() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.setClasspath(Path.systemBootClasspath));
  }

  /**
   * Test {@link LoadProperties#createClasspath()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenLoadProperties_thenReturnLocationFileNameIsNull() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();

    // Act
    Path actualCreateClasspathResult = loadProperties.createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    Path classpath = loadProperties.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertNull(classpath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertEquals(0, classpath.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertFalse(classpath.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link LoadProperties#createClasspath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnProjectIsProject() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    Project project = new Project();
    loadProperties.setProject(project);

    // Act and Assert
    assertSame(project, loadProperties.createClasspath().getProject());
  }

  /**
   * Test {@link LoadProperties#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor).</li>
   *   <li>Then {@link LoadProperties} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenLoadProperties_thenLoadPropertiesClasspathProjectIsNull() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();

    // Act
    loadProperties.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = loadProperties.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link LoadProperties#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link LoadProperties} (default constructor) Classpath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenLoadPropertiesClasspathProjectIsProject() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    Project project = new Project();
    loadProperties.setProject(project);

    // Act
    loadProperties.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = loadProperties.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link LoadProperties#getClasspath()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenLoadProperties_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new LoadProperties()).getClasspath());
  }

  /**
   * Test {@link LoadProperties#getClasspath()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenThrowBuildException() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.getClasspath());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddClassConstantsClassConstants() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addClassConstants(new ClassConstants());
    filter.addFilterReader(new AntFilterReader());

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(filter);
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddClassConstantsClassConstants2() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addClassConstants(new ClassConstants());
    filter.addTokenFilter(new TokenFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(filter);
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addFilterReader {@link AntFilterReader} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddFilterReaderAntFilterReader() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addFilterReader(new AntFilterReader());

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(filter);
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addHeadFilter {@link HeadFilter#HeadFilter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddHeadFilterHeadFilter_thenThrowBuildException() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addHeadFilter(new HeadFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(filter);
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addTokenFilter {@link TokenFilter#TokenFilter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddTokenFilterTokenFilter_thenThrowBuildException() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addTokenFilter(new TokenFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(filter);
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addTokenFilter {@link TokenFilter#TokenFilter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddTokenFilterTokenFilter_thenThrowBuildException2() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addTokenFilter(new TokenFilter());
    filter.addTokenFilter(new TokenFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(filter);
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor) addConfigured {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenLoadPropertiesAddConfiguredPathWithPIsProjectAndPath() throws BuildException {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor) addConfigured {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenLoadPropertiesAddConfiguredPathWithPIsProjectAndPathIsDot() throws BuildException {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenLoadPropertiesAddFilterChainFilterChain_thenThrowBuildException() throws BuildException {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(new FilterChain());
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenLoadPropertiesAddFilterChainFilterChain_thenThrowBuildException2()
      throws BuildException {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addFilterChain(new FilterChain());
    loadProperties.addFilterChain(new FilterChain());
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor) Encoding is {@link Manifest#JAR_ENCODING}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenLoadPropertiesEncodingIsJar_encoding_thenThrowBuildException() throws BuildException {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();
    loadProperties.setEncoding(Manifest.JAR_ENCODING);
    loadProperties.addFilterChain(new FilterChain());
    loadProperties.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenLoadProperties_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new LoadProperties()).execute());
  }

  /**
   * Test {@link LoadProperties#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code Reference Name} and forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#execute()}
   */
  @Test
  public void testExecute_givenProjectAddReferenceReferenceNameAndFortyTwo() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addReference("Reference Name", 42);
    Path a = new Path(p, ".");

    LoadProperties loadProperties = new LoadProperties();
    loadProperties.addConfigured(a);

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.execute());
  }

  /**
   * Test {@link LoadProperties#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor).</li>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenLoadProperties_whenFileList_thenThrowBuildException() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.addConfigured(new FileList()));
  }

  /**
   * Test {@link LoadProperties#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link LoadProperties} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenLoadProperties_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new LoadProperties()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link LoadProperties#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadProperties#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenPathWithProjectIsProject_thenThrowBuildException() {
    // Arrange
    LoadProperties loadProperties = new LoadProperties();

    // Act and Assert
    assertThrows(BuildException.class, () -> loadProperties.addConfigured(new Path(new Project())));
  }

  /**
   * Test new {@link LoadProperties} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LoadProperties}
   */
  @Test
  public void testNewLoadProperties() {
    // Arrange and Act
    LoadProperties actualLoadProperties = new LoadProperties();

    // Assert
    Location location = actualLoadProperties.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLoadProperties.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualLoadProperties.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualLoadProperties.getTaskName());
    assertNull(actualLoadProperties.getTaskType());
    assertNull(actualLoadProperties.getProject());
    assertNull(actualLoadProperties.getOwningTarget());
    assertNull(actualLoadProperties.getClasspath());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualLoadProperties, runtimeConfigurableWrapper.getProxy());
  }
}
