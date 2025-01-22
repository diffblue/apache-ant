package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;
import org.junit.Test;

public class BatchTestDiffblueTest {
  /**
   * Test {@link BatchTest#BatchTest(Project)}.
   * <p>
   * Method under test: {@link BatchTest#BatchTest(Project)}
   */
  @Test
  public void testNewBatchTest() {
    // Arrange
    Project project = new Project();

    // Act
    BatchTest actualBatchTest = new BatchTest(project);

    // Assert
    assertNull(actualBatchTest.destDir);
    assertNull(actualBatchTest.getIfCondition());
    assertNull(actualBatchTest.getUnlessCondition());
    assertNull(actualBatchTest.getErrorProperty());
    assertNull(actualBatchTest.getFailureProperty());
    assertNull(actualBatchTest.getTodir());
    assertNull(actualBatchTest.ifProperty);
    assertNull(actualBatchTest.unlessProperty);
    assertFalse(actualBatchTest.getFork());
    assertFalse(actualBatchTest.getHaltonerror());
    assertFalse(actualBatchTest.getHaltonfailure());
    assertFalse(actualBatchTest.isSkipNonTests());
    assertTrue(project.getBuildListeners().isEmpty());
    assertTrue(actualBatchTest.formatters.isEmpty());
    assertTrue(actualBatchTest.getFiltertrace());
  }

  /**
   * Test {@link BatchTest#addFileSet(FileSet)}.
   * <ul>
   *   <li>Given {@link BatchTest#BatchTest(Project)} with project is {@link Project} (default constructor) addFileSet {@link FileSet#FileSet()}.</li>
   *   <li>When {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BatchTest#addFileSet(FileSet)}
   */
  @Test
  public void testAddFileSet_givenBatchTestWithProjectIsProjectAddFileSetFileSet_whenFileSet() {
    // Arrange
    Project project = new Project();

    BatchTest batchTest = new BatchTest(project);
    batchTest.addFileSet(new FileSet());
    FileSet fs = new FileSet();

    // Act
    batchTest.addFileSet(fs);

    // Assert
    assertSame(project, fs.getProject());
  }

  /**
   * Test {@link BatchTest#addFileSet(FileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link FileSet#FileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BatchTest#addFileSet(FileSet)}
   */
  @Test
  public void testAddFileSet_givenProject_whenFileSetProjectIsProject() {
    // Arrange
    BatchTest batchTest = new BatchTest(new Project());

    FileSet fs = new FileSet();
    Project project = new Project();
    fs.setProject(project);

    // Act
    batchTest.addFileSet(fs);

    // Assert that nothing has changed
    assertSame(project, fs.getProject());
  }

  /**
   * Test {@link BatchTest#addFileSet(FileSet)}.
   * <ul>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link FileSet#FileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BatchTest#addFileSet(FileSet)}
   */
  @Test
  public void testAddFileSet_whenFileSet_thenFileSetProjectIsProject() {
    // Arrange
    Project project = new Project();
    BatchTest batchTest = new BatchTest(project);
    FileSet fs = new FileSet();

    // Act
    batchTest.addFileSet(fs);

    // Assert
    assertSame(project, fs.getProject());
  }

  /**
   * Test {@link BatchTest#javaToClass(String)}.
   * <p>
   * Method under test: {@link BatchTest#javaToClass(String)}
   */
  @Test
  public void testJavaToClass() {
    // Arrange, Act and Assert
    assertEquals("foo.txt", BatchTest.javaToClass("foo.txt"));
  }
}
