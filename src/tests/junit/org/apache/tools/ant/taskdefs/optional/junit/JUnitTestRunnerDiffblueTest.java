package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Permissions;
import org.junit.Test;

public class JUnitTestRunnerDiffblueTest {
  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean)}
   */
  @Test
  public void testNewJUnitTestRunner() {
    // Arrange, Act and Assert
    assertEquals(0, (new JUnitTestRunner(new JUnitTest("Name"), true, true, true)).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, ClassLoader)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, ClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner2() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");

    // Act and Assert
    assertEquals(0, (new JUnitTestRunner(test, true, true, true, new AntClassLoader())).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean)}
   */
  @Test
  public void testNewJUnitTestRunner3() {
    // Arrange, Act and Assert
    assertEquals(0, (new JUnitTestRunner(new JUnitTest("Name"), true, true, true, true)).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean, ClassLoader)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean, ClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner4() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");

    // Act and Assert
    assertEquals(0, (new JUnitTestRunner(test, true, true, true, true, new AntClassLoader())).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean, boolean)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean, boolean)}
   */
  @Test
  public void testNewJUnitTestRunner5() {
    // Arrange, Act and Assert
    assertEquals(0, (new JUnitTestRunner(new JUnitTest("Name"), true, true, true, true, true)).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean, boolean, ClassLoader)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, boolean, boolean, boolean, boolean, boolean, ClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner6() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");

    // Act and Assert
    assertEquals(0, (new JUnitTestRunner(test, true, true, true, true, true, new AntClassLoader())).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean)}.
   * <ul>
   *   <li>When array of {@link String} with {@code Methods}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean)}
   */
  @Test
  public void testNewJUnitTestRunner_whenArrayOfStringWithMethods() {
    // Arrange, Act and Assert
    assertEquals(0, (new JUnitTestRunner(new JUnitTest("Name"), new String[]{"Methods"}, true, true, true, true, true))
        .getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, ClassLoader)}.
   * <ul>
   *   <li>When array of {@link String} with {@code Methods}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, ClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner_whenArrayOfStringWithMethods2() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");

    // Act and Assert
    assertEquals(0,
        (new JUnitTestRunner(test, new String[]{"Methods"}, true, true, true, true, true, new AntClassLoader()))
            .getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean)}
   */
  @Test
  public void testNewJUnitTestRunner_whenNull() {
    // Arrange, Act and Assert
    assertEquals(0, (new JUnitTestRunner(new JUnitTest("Name"), null, true, true, true, true, true)).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, ClassLoader)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#JUnitTestRunner(JUnitTest, String[], boolean, boolean, boolean, boolean, boolean, ClassLoader)}
   */
  @Test
  public void testNewJUnitTestRunner_whenNull2() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");

    // Act and Assert
    assertEquals(0, (new JUnitTestRunner(test, null, true, true, true, true, true, new AntClassLoader())).getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true);

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun2() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("java.util.Properties"), true, true, true);

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun3() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(test, true, true, true, new AntClassLoader());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun4() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true);
    jUnitTestRunner.addFormatter(new BriefJUnitResultFormatter());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun5() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("java.util.Properties"), false, true, true);

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun6() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("java.util.Properties"), true, true, true);
    jUnitTestRunner.addFormatter(new BriefJUnitResultFormatter());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun7() {
    // Arrange
    JUnitTest test = new JUnitTest("java.util.Properties");
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(test, true, true, true, new AntClassLoader());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun8() {
    // Arrange
    JUnitTest test = new JUnitTest("Name");
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(test, true, true, true,
        new AntClassLoader(new AntClassLoader(), true));

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun9() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true, loader);

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun10() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Finding class ").toFile());
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true, loader);

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun11() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, false, true);
    jUnitTestRunner.addFormatter(new BriefJUnitResultFormatter());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun12() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true);
    jUnitTestRunner.addFormatter(new XMLJUnitResultFormatter());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun13() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true);
    jUnitTestRunner.addFormatter(new TearDownOnVmCrash());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun14() {
    // Arrange
    JUnitTest test = new JUnitTest("java.util.Properties");
    test.setSkipNonTests(true);
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(test, true, true, true);

    // Act
    jUnitTestRunner.run();

    // Assert that nothing has changed
    assertEquals(0, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun15() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("java.util.Properties"), true, true, true);
    jUnitTestRunner.addFormatter(new PlainJUnitResultFormatter());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun16() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("java.util.Properties"), true, true, true);
    jUnitTestRunner.addFormatter(new FailureRecorder());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun17() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("java.util.Properties"), true, true, true);
    jUnitTestRunner.addFormatter(new XMLJUnitResultFormatter());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun_givenAntClassLoaderProjectIsProject() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.setProject(new Project());
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true, loader);

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#run()}.
   * <ul>
   *   <li>Given {@link JUnitTest#JUnitTest(String)} with name is {@code .class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#run()}
   */
  @Test
  public void testRun_givenJUnitTestWithNameIsClass() {
    // Arrange
    JUnitTest test = new JUnitTest(".class");
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(test, true, true, true, new AntClassLoader());

    // Act
    jUnitTestRunner.run();

    // Assert
    assertEquals(2, jUnitTestRunner.getRetCode());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link JUnitTestRunner#setPermissions(Permissions)}
   *   <li>{@link JUnitTestRunner#getRetCode()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true);

    // Act
    jUnitTestRunner.setPermissions(new Permissions());

    // Assert
    assertEquals(0, jUnitTestRunner.getRetCode());
  }

  /**
   * Test {@link JUnitTestRunner#handleInput(byte[], int, int)}.
   * <p>
   * Method under test: {@link JUnitTestRunner#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput() throws IOException {
    // Arrange
    JUnitTestRunner jUnitTestRunner = new JUnitTestRunner(new JUnitTest("Name"), true, true, true);

    // Act and Assert
    assertEquals(-1, jUnitTestRunner.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link JUnitTestRunner#filterStack(String)}.
   * <ul>
   *   <li>When {@code Stack}.</li>
   *   <li>Then return {@code Stack}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitTestRunner#filterStack(String)}
   */
  @Test
  public void testFilterStack_whenStack_thenReturnStack() {
    // Arrange, Act and Assert
    assertEquals("Stack\n", JUnitTestRunner.filterStack("Stack"));
  }
}
