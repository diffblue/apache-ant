package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
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
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class LoadResourceDiffblueTest {
  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddClassConstantsClassConstants() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addClassConstants(new ClassConstants());
    filter.addFilterReader(new AntFilterReader());

    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(filter);
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddClassConstantsClassConstants2() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addClassConstants(new ClassConstants());
    filter.addTokenFilter(new TokenFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(filter);
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addFilterReader {@link AntFilterReader} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddFilterReaderAntFilterReader() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addFilterReader(new AntFilterReader());

    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(filter);
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addHeadFilter {@link HeadFilter#HeadFilter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddHeadFilterHeadFilter_thenThrowBuildException() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addHeadFilter(new HeadFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(filter);
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addTokenFilter {@link TokenFilter#TokenFilter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddTokenFilterTokenFilter_thenThrowBuildException() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addTokenFilter(new TokenFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(filter);
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor) addTokenFilter {@link TokenFilter#TokenFilter()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenFilterChainAddTokenFilterTokenFilter_thenThrowBuildException2() throws BuildException {
    // Arrange
    FilterChain filter = new FilterChain();
    filter.addTokenFilter(new TokenFilter());
    filter.addTokenFilter(new TokenFilter());
    filter.addFilterReader(new AntFilterReader());

    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(filter);
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) addConfigured {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceAddConfiguredPathWithPIsProjectAndPath() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) addConfigured {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceAddConfiguredPathWithPIsProjectAndPath2() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "Path"));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) addConfigured {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceAddConfiguredPathWithPIsProjectAndPathIsDot() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceAddFilterChainFilterChain_thenThrowBuildException() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(new FilterChain());
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceAddFilterChainFilterChain_thenThrowBuildException2() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.addFilterChain(new FilterChain());
    loadResource.addFilterChain(new FilterChain());
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) Encoding is {@link Manifest#JAR_ENCODING}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceEncodingIsJar_encoding_thenThrowBuildException() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.setEncoding(Manifest.JAR_ENCODING);
    loadResource.addFilterChain(new FilterChain());
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResourceProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    LoadResource loadResource = new LoadResource();
    loadResource.setProject(new Project());
    loadResource.addFilterChain(new FilterChain());
    loadResource.setProperty("output property not defined");
    loadResource.addConfigured(new Path(new Project(), "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.execute());
  }

  /**
   * Test {@link LoadResource#execute()}.
   * <ul>
   *   <li>Given {@link LoadResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#execute()}
   */
  @Test
  public void testExecute_givenLoadResource_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new LoadResource()).execute());
  }

  /**
   * Test {@link LoadResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenFileList_thenThrowBuildException() {
    // Arrange
    LoadResource loadResource = new LoadResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> loadResource.addConfigured(new FileList()));
  }

  /**
   * Test {@link LoadResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoadResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new LoadResource()).addConfigured(Resources.NONE));
  }

  /**
   * Test new {@link LoadResource} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LoadResource}
   */
  @Test
  public void testNewLoadResource() {
    // Arrange and Act
    LoadResource actualLoadResource = new LoadResource();

    // Assert
    Location location = actualLoadResource.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLoadResource.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualLoadResource.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualLoadResource.getTaskName());
    assertNull(actualLoadResource.getTaskType());
    assertNull(actualLoadResource.getProject());
    assertNull(actualLoadResource.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualLoadResource, runtimeConfigurableWrapper.getProxy());
  }
}
