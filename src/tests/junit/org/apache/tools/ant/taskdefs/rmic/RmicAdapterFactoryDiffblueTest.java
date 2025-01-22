package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class RmicAdapterFactoryDiffblueTest {
  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task, Path)} with {@code rmicType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link KaffeRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link KaffeRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task, Path)}
   */
  @Test
  public void testGetRmicWithRmicTypeTaskClasspath_whenCompiler_name_thenReturnKaffeRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(KaffeRmic.COMPILER_NAME, task, Path.systemBootClasspath);

    // Assert
    assertTrue(actualRmic instanceof KaffeRmic);
    assertNull(((KaffeRmic) actualRmic).getRmic());
    assertNull(actualRmic.getMapper());
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, ((KaffeRmic) actualRmic).getTieClassSuffix());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task, Path)} with {@code rmicType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link SunRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link SunRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task, Path)}
   */
  @Test
  public void testGetRmicWithRmicTypeTaskClasspath_whenCompiler_name_thenReturnSunRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(SunRmic.COMPILER_NAME, task, Path.systemBootClasspath);

    // Assert
    assertTrue(actualRmic instanceof SunRmic);
    assertNull(((SunRmic) actualRmic).getRmic());
    assertNull(actualRmic.getMapper());
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, ((SunRmic) actualRmic).getTieClassSuffix());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task, Path)} with {@code rmicType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link WLRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link WLRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task, Path)}
   */
  @Test
  public void testGetRmicWithRmicTypeTaskClasspath_whenCompiler_name_thenReturnWLRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(WLRmic.COMPILER_NAME, task, Path.systemBootClasspath);

    // Assert
    assertTrue(actualRmic instanceof WLRmic);
    assertNull(((WLRmic) actualRmic).getRmic());
    assertNull(actualRmic.getMapper());
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, ((WLRmic) actualRmic).getTieClassSuffix());
    assertEquals(WLRmic.WL_RMI_SKEL_SUFFIX, ((WLRmic) actualRmic).getSkelClassSuffix());
    assertEquals(WLRmic.WL_RMI_STUB_SUFFIX, ((WLRmic) actualRmic).getStubClassSuffix());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task, Path)} with {@code rmicType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link XNewRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link XNewRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task, Path)}
   */
  @Test
  public void testGetRmicWithRmicTypeTaskClasspath_whenCompiler_name_thenReturnXNewRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(XNewRmic.COMPILER_NAME, task, Path.systemBootClasspath);

    // Assert
    assertTrue(actualRmic instanceof XNewRmic);
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, ((XNewRmic) actualRmic).getSkelClassSuffix());
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, ((XNewRmic) actualRmic).getStubClassSuffix());
    assertEquals(SunRmic.RMIC_EXECUTABLE, ((XNewRmic) actualRmic).getExecutableName());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task, Path)} with {@code rmicType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link RmicAdapterFactory#DEFAULT_COMPILER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task, Path)}
   */
  @Test
  public void testGetRmicWithRmicTypeTaskClasspath_whenDefault_compiler() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(RmicAdapterFactory.DEFAULT_COMPILER, task,
        Path.systemBootClasspath);

    // Assert
    assertTrue(actualRmic instanceof ForkingSunRmic);
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, ((ForkingSunRmic) actualRmic).getSkelClassSuffix());
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, ((ForkingSunRmic) actualRmic).getStubClassSuffix());
    assertEquals(SunRmic.RMIC_EXECUTABLE, ((ForkingSunRmic) actualRmic).getExecutableName());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task, Path)} with {@code rmicType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@link ForkingSunRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task, Path)}
   */
  @Test
  public void testGetRmicWithRmicTypeTaskClasspath_whenEmptyString_thenReturnForkingSunRmic() throws BuildException {
    // Arrange and Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic("", new TaskAdapter(), Path.systemBootClasspath);

    // Assert
    assertTrue(actualRmic instanceof ForkingSunRmic);
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, ((ForkingSunRmic) actualRmic).getSkelClassSuffix());
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, ((ForkingSunRmic) actualRmic).getStubClassSuffix());
    assertEquals(SunRmic.RMIC_EXECUTABLE, ((ForkingSunRmic) actualRmic).getExecutableName());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task)} with {@code rmicType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link KaffeRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link KaffeRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task)}
   */
  @Test
  public void testGetRmicWithRmicTypeTask_givenProject_whenCompiler_name_thenReturnKaffeRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(KaffeRmic.COMPILER_NAME, task);

    // Assert
    assertTrue(actualRmic instanceof KaffeRmic);
    assertNull(((KaffeRmic) actualRmic).getRmic());
    assertNull(actualRmic.getMapper());
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, ((KaffeRmic) actualRmic).getTieClassSuffix());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task)} with {@code rmicType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link SunRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link SunRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task)}
   */
  @Test
  public void testGetRmicWithRmicTypeTask_givenProject_whenCompiler_name_thenReturnSunRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(SunRmic.COMPILER_NAME, task);

    // Assert
    assertTrue(actualRmic instanceof SunRmic);
    assertNull(((SunRmic) actualRmic).getRmic());
    assertNull(actualRmic.getMapper());
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, ((SunRmic) actualRmic).getTieClassSuffix());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task)} with {@code rmicType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link WLRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link WLRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task)}
   */
  @Test
  public void testGetRmicWithRmicTypeTask_givenProject_whenCompiler_name_thenReturnWLRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(WLRmic.COMPILER_NAME, task);

    // Assert
    assertTrue(actualRmic instanceof WLRmic);
    assertNull(((WLRmic) actualRmic).getRmic());
    assertNull(actualRmic.getMapper());
    assertEquals(DefaultRmicAdapter.RMI_TIE_SUFFIX, ((WLRmic) actualRmic).getTieClassSuffix());
    assertEquals(WLRmic.WL_RMI_SKEL_SUFFIX, ((WLRmic) actualRmic).getSkelClassSuffix());
    assertEquals(WLRmic.WL_RMI_STUB_SUFFIX, ((WLRmic) actualRmic).getStubClassSuffix());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task)} with {@code rmicType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link XNewRmic#COMPILER_NAME}.</li>
   *   <li>Then return {@link XNewRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task)}
   */
  @Test
  public void testGetRmicWithRmicTypeTask_givenProject_whenCompiler_name_thenReturnXNewRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(XNewRmic.COMPILER_NAME, task);

    // Assert
    assertTrue(actualRmic instanceof XNewRmic);
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, ((XNewRmic) actualRmic).getSkelClassSuffix());
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, ((XNewRmic) actualRmic).getStubClassSuffix());
    assertEquals(SunRmic.RMIC_EXECUTABLE, ((XNewRmic) actualRmic).getExecutableName());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task)} with {@code rmicType}, {@code task}.
   * <ul>
   *   <li>When {@link RmicAdapterFactory#DEFAULT_COMPILER}.</li>
   *   <li>Then return {@link ForkingSunRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task)}
   */
  @Test
  public void testGetRmicWithRmicTypeTask_whenDefault_compiler_thenReturnForkingSunRmic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic(RmicAdapterFactory.DEFAULT_COMPILER, task);

    // Assert
    assertTrue(actualRmic instanceof ForkingSunRmic);
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, ((ForkingSunRmic) actualRmic).getSkelClassSuffix());
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, ((ForkingSunRmic) actualRmic).getStubClassSuffix());
    assertEquals(SunRmic.RMIC_EXECUTABLE, ((ForkingSunRmic) actualRmic).getExecutableName());
  }

  /**
   * Test {@link RmicAdapterFactory#getRmic(String, Task)} with {@code rmicType}, {@code task}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@link ForkingSunRmic}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RmicAdapterFactory#getRmic(String, Task)}
   */
  @Test
  public void testGetRmicWithRmicTypeTask_whenEmptyString_thenReturnForkingSunRmic() throws BuildException {
    // Arrange and Act
    RmicAdapter actualRmic = RmicAdapterFactory.getRmic("", new TaskAdapter());

    // Assert
    assertTrue(actualRmic instanceof ForkingSunRmic);
    assertEquals(DefaultRmicAdapter.RMI_SKEL_SUFFIX, ((ForkingSunRmic) actualRmic).getSkelClassSuffix());
    assertEquals(DefaultRmicAdapter.RMI_STUB_SUFFIX, ((ForkingSunRmic) actualRmic).getStubClassSuffix());
    assertEquals(SunRmic.RMIC_EXECUTABLE, ((ForkingSunRmic) actualRmic).getExecutableName());
  }
}
