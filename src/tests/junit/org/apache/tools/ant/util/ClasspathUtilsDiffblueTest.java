package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Hashtable;
import junit.runner.TestCaseClassLoader;
import org.apache.ant.antunit.AntUnit;
import org.apache.bsf.util.event.generator.AdapterClassLoader;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class ClasspathUtilsDiffblueTest {
  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link ClasspathUtils.Delegate#Delegate(ProjectComponent)}
   *   <li>{@link ClasspathUtils.Delegate#setClassname(String)}
   *   <li>{@link ClasspathUtils.Delegate#setReverseLoader(boolean)}
   *   <li>{@link ClasspathUtils.Delegate#getClasspath()}
   *   <li>{@link ClasspathUtils.Delegate#isReverseLoader()}
   * </ul>
   */
  @Test
  public void testDelegateConstructor() {
    // Arrange and Act
    ClasspathUtils.Delegate actualDelegate = new ClasspathUtils.Delegate(new AntUnit());
    actualDelegate.setClassname("Fcqn");
    actualDelegate.setReverseLoader(true);

    // Assert
    assertNull(actualDelegate.getClasspath());
    assertTrue(actualDelegate.isReverseLoader());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#createClasspath()}
   */
  @Test
  public void testDelegateCreateClasspath() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());

    // Act
    Path actualCreateClasspathResult = delegate.createClasspath();

    // Assert
    assertEquals(0, actualCreateClasspathResult.size());
    assertNull(actualCreateClasspathResult.getProject());
    Path classpath = delegate.getClasspath();
    assertEquals(0, classpath.size());
    assertTrue(classpath.isEmpty());
    assertNull(classpath.getProject());
    Location expectedLocation = actualCreateClasspathResult.getLocation();
    assertSame(expectedLocation, classpath.getLocation());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#createClasspath()}
   */
  @Test
  public void testDelegateCreateClasspath2() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());

    // Act and Assert
    assertEquals(0, ClasspathUtils.getDelegate(antUnit).createClasspath().size());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#createClasspath()}
   */
  @Test
  public void testDelegateCreateClasspath3() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspath(new Path(new Project()));

    // Act and Assert
    assertEquals(0, delegate.createClasspath().size());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#createClasspath()}
   */
  @Test
  public void testDelegateCreateClasspath4() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspathref(new Reference("42"));

    // Act
    Path actualCreateClasspathResult = delegate.createClasspath();

    // Assert
    assertEquals(0, actualCreateClasspathResult.size());
    assertNull(actualCreateClasspathResult.getProject());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoadId()}
   */
  @Test
  public void testDelegateGetClassLoadId() {
    // Arrange, Act and Assert
    assertNull(ClasspathUtils.getDelegate(new AntUnit()).getClassLoadId());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoadId()}
   */
  @Test
  public void testDelegateGetClassLoadId2() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspathref(new Reference("42"));

    // Act and Assert
    assertEquals("ant.loader.42", delegate.getClassLoadId());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoadId()}
   */
  @Test
  public void testDelegateGetClassLoadId3() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setLoaderRef(new Reference("42"));

    // Act and Assert
    assertEquals("42", delegate.getClassLoadId());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    antUnit.setProject(new Project());

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getDelegate(antUnit).getClassLoader()).getClasspath());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    AntUnit antUnit = new AntUnit();
    antUnit.setProject(project);

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getDelegate(antUnit).getClassLoader()).getClasspath());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader3() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition(ClasspathUtils.REUSE_LOADER_REF, Object.class);
    project.addBuildListener(new AntClassLoader());

    AntUnit antUnit = new AntUnit();
    antUnit.setProject(project);

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getDelegate(antUnit).getClassLoader()).getClasspath());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader4() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget(ClasspathUtils.REUSE_LOADER_REF, new Target());
    project.addBuildListener(new AntClassLoader());

    AntUnit antUnit = new AntUnit();
    antUnit.setProject(project);

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getDelegate(antUnit).getClassLoader()).getClasspath());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setClasspath(Path)}
   */
  @Test
  public void testDelegateSetClasspath() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspathref(new Reference("42"));
    Path path = new Path(null);

    // Act
    delegate.setClasspath(path);

    // Assert
    assertNull(path.getProject());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setClasspath(Path)}
   */
  @Test
  public void testDelegateSetClasspath2() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspathref(new Reference("42"));

    // Act
    delegate.setClasspath(null);

    // Assert that nothing has changed
    assertEquals("ant.loader.42", delegate.getClassLoadId());
    assertFalse(delegate.isReverseLoader());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setClasspathref(Reference)}
   */
  @Test
  public void testDelegateSetClasspathref() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());

    // Act
    delegate.setClasspathref(new Reference("42"));

    // Assert
    assertEquals("ant.loader.42", delegate.getClassLoadId());
    assertNull(delegate.getClasspath().getProject());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setClasspathref(Reference)}
   */
  @Test
  public void testDelegateSetClasspathref2() {
    // Arrange
    AntUnit antUnit = new AntUnit();
    Project project = new Project();
    antUnit.setProject(project);
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(antUnit);

    // Act
    delegate.setClasspathref(new Reference("42"));

    // Assert
    assertEquals("ant.loader.42", delegate.getClassLoadId());
    assertSame(project, delegate.getClasspath().getProject());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setClasspathref(Reference)}
   */
  @Test
  public void testDelegateSetClasspathref3() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspath(new Path(new Project()));

    // Act
    delegate.setClasspathref(new Reference("42"));

    // Assert
    assertEquals("ant.loader.42", delegate.getClassLoadId());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setClasspathref(Reference)}
   */
  @Test
  public void testDelegateSetClasspathref4() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());
    delegate.setClasspath(new Path(new Project(), "Path"));

    // Act
    delegate.setClasspathref(new Reference("42"));

    // Assert
    assertEquals("ant.loader.42", delegate.getClassLoadId());
  }

  /**
   * Method under test: {@link ClasspathUtils.Delegate#setLoaderRef(Reference)}
   */
  @Test
  public void testDelegateSetLoaderRef() {
    // Arrange
    ClasspathUtils.Delegate delegate = ClasspathUtils.getDelegate(new AntUnit());

    // Act
    delegate.setLoaderRef(new Reference("42"));

    // Assert
    assertEquals("42", delegate.getClassLoadId());
  }

  /**
  * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String)}
  */
  @Test
  public void testGetClassLoaderForPath() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition(ClasspathUtils.REUSE_LOADER_REF, Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("",
        ((AntClassLoader) ClasspathUtils.getClassLoaderForPath(project, new Path(new Project()), "42")).getClasspath());
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath2() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition(ClasspathUtils.REUSE_LOADER_REF, Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("",
        ((AntClassLoader) ClasspathUtils.getClassLoaderForPath(project, new Path(new Project()), "42", true))
            .getClasspath());
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath3() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("ant.PropertyHelper", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("",
        ((AntClassLoader) ClasspathUtils.getClassLoaderForPath(project, new Path(new Project()), "42", true, true))
            .getClasspath());
    assertEquals(2, project.getBuildListeners().size());
    Hashtable<String, Target> expectedUserProperties = project.getTargets();
    assertEquals(expectedUserProperties, project.getUserProperties());
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath4() {
    // Arrange
    Project project = new Project();
    project.addReference("42", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ClasspathUtils.getClassLoaderForPath(project, new Path(new Project()), "42", true, true));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPath5() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42")));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPath6() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42")));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPath7() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("ant.PropertyHelper", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42")));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPath8() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42")));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPath9() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42")));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath10() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42"), true));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath11() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42"), true));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath12() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("ant.PropertyHelper", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42"), true));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath13() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42"), true));
  }

  /**
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPath14() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(project, new Reference("42"), true));
  }

  /**
   * Method under test: {@link ClasspathUtils#getDelegate(ProjectComponent)}
   */
  @Test
  public void testGetDelegate() {
    // Arrange, Act and Assert
    assertFalse(ClasspathUtils.getDelegate(new AntUnit()).isReverseLoader());
  }

  /**
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertEquals("",
        ((AntClassLoader) ClasspathUtils.getUniqueClassLoaderForPath(project, new Path(new Project()), true))
            .getClasspath());
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("",
        ((AntClassLoader) ClasspathUtils.getUniqueClassLoaderForPath(project, new Path(new Project()), true))
            .getClasspath());
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath3() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertEquals("",
        ((AntClassLoader) ClasspathUtils.getUniqueClassLoaderForPath(project, new Path(null), true)).getClasspath());
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath4() {
    // Arrange
    Project project = new Project();

    Project project1 = new Project();
    project1.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getUniqueClassLoaderForPath(project, new Path(project1), true))
        .getClasspath());
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath5() {
    // Arrange
    Project project = new Project();

    Project project1 = new Project();
    project1.addDataTypeDefinition("ignore", Object.class);
    project1.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getUniqueClassLoaderForPath(project, new Path(project1), true))
        .getClasspath());
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath6() throws BuildException {
    // Arrange
    Project project = new Project();

    Project project1 = new Project();
    project1.addTarget("ignore", new Target());
    project1.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertEquals("", ((AntClassLoader) ClasspathUtils.getUniqueClassLoaderForPath(project, new Path(project1), true))
        .getClasspath());
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", new TestCaseClassLoader()));
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("", new TestCaseClassLoader()));
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", new AntClassLoader()));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance2() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name",
        new SplitClassLoader(parent, path, new Project(), new String[]{"Class not found: "})));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance3() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class not found: ",
        new SplitClassLoader(parent, path, new Project(), new String[]{"Class not found: "})));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance4() {
    // Arrange
    AdapterClassLoader parent = new AdapterClassLoader();
    Path path = new Path(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name",
        new SplitClassLoader(parent, path, new Project(), new String[]{"Class not found: "})));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance5() {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    Path path = new Path(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name",
        new SplitClassLoader(parent, path, new Project(), new String[]{"Class not found: "})));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance6() {
    // Arrange
    Path path = new Path(new Project());
    path.addJavaRuntime();
    TestCaseClassLoader parent = new TestCaseClassLoader();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name",
        new SplitClassLoader(parent, path, new Project(), new String[]{"Class not found: "})));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance7() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("Class Name", Object.class);
    TestCaseClassLoader parent = new TestCaseClassLoader();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name",
        new SplitClassLoader(parent, new Path(new Project()), project, new String[]{"Class not found: "})));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance8() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    org.apache.tools.ant.types.Path path = new org.apache.tools.ant.types.Path(new Project());

    SplitClassLoader splitClassLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});
    splitClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", splitClassLoader));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstance9() throws BuildException {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());

    SplitClassLoader splitClassLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});
    splitClassLoader.addPathElement(".");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", splitClassLoader));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance10() {
    // Arrange
    TestCaseClassLoader userDefinedLoader = new TestCaseClassLoader();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance11() {
    // Arrange
    TestCaseClassLoader userDefinedLoader = new TestCaseClassLoader();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance12() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());
    SplitClassLoader userDefinedLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance13() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance14() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());
    SplitClassLoader userDefinedLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ClasspathUtils.newInstance("Class not found: ", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance15() {
    // Arrange
    AdapterClassLoader parent = new AdapterClassLoader();
    Path path = new Path(new Project());
    SplitClassLoader userDefinedLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance16() {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    Path path = new Path(new Project());
    SplitClassLoader userDefinedLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance17() {
    // Arrange
    Path path = new Path(new Project());
    path.addJavaRuntime();
    TestCaseClassLoader parent = new TestCaseClassLoader();
    SplitClassLoader userDefinedLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance18() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("Class Name", Object.class);
    TestCaseClassLoader parent = new TestCaseClassLoader();
    SplitClassLoader userDefinedLoader = new SplitClassLoader(parent, new Path(new Project()), project,
        new String[]{"Class not found: "});

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance19() {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    org.apache.tools.ant.types.Path path = new org.apache.tools.ant.types.Path(new Project());

    SplitClassLoader splitClassLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});
    splitClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", splitClassLoader, Object.class));
  }

  /**
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstance20() throws BuildException {
    // Arrange
    TestCaseClassLoader parent = new TestCaseClassLoader();
    Path path = new Path(new Project());

    SplitClassLoader splitClassLoader = new SplitClassLoader(parent, path, new Project(),
        new String[]{"Class not found: "});
    splitClassLoader.addPathElement(".");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", splitClassLoader, Object.class));
  }
}

