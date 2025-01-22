package org.apache.tools.ant.util.depend;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.depend.AntAnalyzer;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PolyTest;
import org.apache.tools.ant.types.PolyTest.MyPath;
import org.junit.Test;

public class AbstractAnalyzerDiffblueTest {
  /**
   * Test {@link AbstractAnalyzer#getClassContainer(String)}.
   * <p>
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project(), ".class"));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getClassContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addClassPath {@link PolyTest.MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer_givenAntAnalyzerAddClassPathMyPathWithProjectIsProject() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new MyPath(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getClassContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addClassPath {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer_givenAntAnalyzerAddClassPathPathWithPIsProjectAndPathIsDot() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project(), "."));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getClassContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addClassPath {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer_givenAntAnalyzerAddClassPathPathWithProjectIsProject() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(new Path(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getClassContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer_givenAntAnalyzer_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new AntAnalyzer()).getClassContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getClassContainer(String)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getClassContainer(String)}
   */
  @Test
  public void testGetClassContainer_givenPathWithProjectIsProjectAddFilelistFileList()
      throws IOException, BuildException {
    // Arrange
    Path classPath = new Path(new Project());
    classPath.addFilelist(new FileList());

    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addClassPath(classPath);

    // Act and Assert
    assertNull(antAnalyzer.getClassContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getSourceContainer(String)}.
   * <p>
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new Path(new Project(), ".java"));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getSourceContainer(String)}.
   * <p>
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer2() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new Path(new Project(), "."));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getSourceContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addSourcePath {@link PolyTest.MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer_givenAntAnalyzerAddSourcePathMyPathWithProjectIsProject() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new MyPath(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getSourceContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) addSourcePath {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer_givenAntAnalyzerAddSourcePathPathWithProjectIsProject() throws IOException {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(new Path(new Project()));

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getSourceContainer(String)}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer_givenAntAnalyzer_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new AntAnalyzer()).getSourceContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#getSourceContainer(String)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#getSourceContainer(String)}
   */
  @Test
  public void testGetSourceContainer_givenPathWithProjectIsProjectAddFilelistFileList()
      throws IOException, BuildException {
    // Arrange
    Path sourcePath = new Path(new Project());
    sourcePath.addFilelist(new FileList());

    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.addSourcePath(sourcePath);

    // Act and Assert
    assertNull(antAnalyzer.getSourceContainer("Classname"));
  }

  /**
   * Test {@link AbstractAnalyzer#isClosureRequired()}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor) Closure is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#isClosureRequired()}
   */
  @Test
  public void testIsClosureRequired_givenAntAnalyzerClosureIsFalse_thenReturnFalse() {
    // Arrange
    AntAnalyzer antAnalyzer = new AntAnalyzer();
    antAnalyzer.setClosure(false);

    // Act and Assert
    assertFalse(antAnalyzer.isClosureRequired());
  }

  /**
   * Test {@link AbstractAnalyzer#isClosureRequired()}.
   * <ul>
   *   <li>Given {@link AntAnalyzer} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAnalyzer#isClosureRequired()}
   */
  @Test
  public void testIsClosureRequired_givenAntAnalyzer_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new AntAnalyzer()).isClosureRequired());
  }
}
