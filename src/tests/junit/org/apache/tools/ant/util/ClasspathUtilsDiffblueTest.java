package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.util.ClasspathUtils.Delegate;
import org.apache.tools.ant.util.facade.ImplementationSpecificArgument;
import org.junit.Test;

public class ClasspathUtilsDiffblueTest {
  /**
   * Test Delegate {@link Delegate#createClasspath()}.
   * <p>
   * Method under test: {@link Delegate#createClasspath()}
   */
  @Test
  public void testDelegateCreateClasspath() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = delegate.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, delegate.getClasspath());
  }

  /**
   * Test Delegate {@link Delegate#createClasspath()}.
   * <p>
   * Method under test: {@link Delegate#createClasspath()}
   */
  @Test
  public void testDelegateCreateClasspath2() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(new ImplementationSpecificArgument());

    // Act
    Path actualCreateClasspathResult = delegate.createClasspath();

    // Assert
    Path classpath = delegate.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoadId()}.
   * <ul>
   *   <li>Given Delegate is {@link Path#systemBootClasspath}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoadId()}
   */
  @Test
  public void testDelegateGetClassLoadId_givenDelegateIsSystemBootClasspath_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(ClasspathUtils.getDelegate(Path.systemBootClasspath).getClassLoadId());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoadId()}.
   * <ul>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoadId()}
   */
  @Test
  public void testDelegateGetClassLoadId_thenReturn42() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setLoaderRef(new Reference("42"));
    delegate.setClasspathref(new Reference("42"));

    // Act and Assert
    assertEquals("42", delegate.getClassLoadId());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoadId()}.
   * <ul>
   *   <li>Then return {@code ant.loader.42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoadId()}
   */
  @Test
  public void testDelegateGetClassLoadId_thenReturnAntLoader42() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspathref(new Reference("42"));

    // Act and Assert
    assertEquals("ant.loader.42", delegate.getClassLoadId());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setLoaderRef(new Reference("42"));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader2() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(new Path(new Project()));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader3() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(new Path(new Project(), ClasspathUtils.REUSE_LOADER_REF));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader4() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setLoaderRef(new Reference(new Project(), "42"));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader5() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(new Path(null));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader6() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(new Path(null, ClasspathUtils.REUSE_LOADER_REF));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader7() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(new Path(new Project(), "*"));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader8() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(new Path(new Project(), "."));

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <ul>
   *   <li>Given Delegate is {@link Path#systemBootClasspath} ReverseLoader is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader_givenDelegateIsSystemBootClasspathReverseLoaderIsTrue() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setReverseLoader(true);

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <ul>
   *   <li>Given Delegate is {@link Path#systemBootClasspath}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader_givenDelegateIsSystemBootClasspath_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(ClasspathUtils.getDelegate(Path.systemBootClasspath).getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFilelist(new FileList());
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(classpath);

    // Act and Assert
    assertNotNull(delegate.getClassLoader());
  }

  /**
   * Test Delegate {@link Delegate#getClassLoader()}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#getClassLoader()}
   */
  @Test
  public void testDelegateGetClassLoader_givenTaskAdapterProjectIsProject_thenReturnNotNull() {
    // Arrange
    TaskAdapter component = new TaskAdapter();
    component.setProject(new Project());

    // Act and Assert
    assertNotNull(ClasspathUtils.getDelegate(component).getClassLoader());
  }

  /**
   * Test Delegate getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Delegate#Delegate(ProjectComponent)}
   *   <li>{@link Delegate#setClassname(String)}
   *   <li>{@link Delegate#setReverseLoader(boolean)}
   *   <li>{@link Delegate#getClasspath()}
   *   <li>{@link Delegate#isReverseLoader()}
   * </ul>
   */
  @Test
  public void testDelegateGettersAndSetters() {
    // Arrange and Act
    Delegate actualDelegate = new Delegate(Path.systemBootClasspath);
    actualDelegate.setClassname("Fcqn");
    actualDelegate.setReverseLoader(true);
    Path actualClasspath = actualDelegate.getClasspath();

    // Assert
    assertNull(actualClasspath);
    assertTrue(actualDelegate.isReverseLoader());
  }

  /**
   * Test Delegate {@link Delegate#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#setClasspath(Path)}
   */
  @Test
  public void testDelegateSetClasspath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    Project project = new Project();
    delegate.setClasspath(new Path(project));
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    // Act
    delegate.setClasspath(classpath);

    // Assert
    assertSame(project, classpath.getProject());
  }

  /**
   * Test Delegate {@link Delegate#setClasspathref(Reference)}.
   * <p>
   * Method under test: {@link Delegate#setClasspathref(Reference)}
   */
  @Test
  public void testDelegateSetClasspathref() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);
    delegate.setClasspath(Path.systemBootClasspath);

    // Act
    delegate.setClasspathref(new Reference("42"));

    // Assert that nothing has changed
    Path classpath = delegate.getClasspath();
    Location location = classpath.getLocation();
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(classpath.isReference());
  }

  /**
   * Test Delegate {@link Delegate#setClasspathref(Reference)}.
   * <p>
   * Method under test: {@link Delegate#setClasspathref(Reference)}
   */
  @Test
  public void testDelegateSetClasspathref2() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(new ImplementationSpecificArgument());

    // Act
    delegate.setClasspathref(new Reference("42"));

    // Assert
    Path classpath = delegate.getClasspath();
    Location location = classpath.getLocation();
    assertNull(location.getFileName());
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(classpath.isReference());
  }

  /**
   * Test Delegate {@link Delegate#setLoaderRef(Reference)}.
   * <ul>
   *   <li>Then Delegate is {@link Path#systemBootClasspath} ClassLoadId is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Delegate#setLoaderRef(Reference)}
   */
  @Test
  public void testDelegateSetLoaderRef_thenDelegateIsSystemBootClasspathClassLoadIdIs42() {
    // Arrange
    Delegate delegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);

    // Act
    delegate.setLoaderRef(new Reference("42"));

    // Assert
    assertEquals("42", delegate.getClassLoadId());
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean, boolean)} with {@code p}, {@code path}, {@code loaderId}, {@code reverseLoader}, {@code reuseLoader}.
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPPathLoaderIdReverseLoaderReuseLoader() {
    // Arrange
    Project p = new Project();
    p.addReference("42", "Value");
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ClasspathUtils.getClassLoaderForPath(p, Path.systemBootClasspath, "42", true, true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean, boolean)} with {@code p}, {@code path}, {@code loaderId}, {@code reverseLoader}, {@code reuseLoader}.
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Path, String, boolean, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPPathLoaderIdReverseLoaderReuseLoader2() throws MalformedURLException {
    // Arrange
    Project p = new Project();
    p.addReference("42",
        new URLClassLoader(new URL[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()}));
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(ClasspathUtils.getClassLoaderForPath(p, Path.systemBootClasspath, "42", true, true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_givenAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42"), true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>Given empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_givenEmptyString() {
    // Arrange
    Project p = new Project();

    Reference ref = new Reference("42");
    ref.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, ref, false));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("42", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42"), true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>Given {@code Ref}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_givenRef() {
    // Arrange
    Project p = new Project();

    Reference ref = new Reference("42");
    ref.setRefId("Ref");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, ref, false));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_givenTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("42", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42"), true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_givenValue() {
    // Arrange
    Project p = new Project();
    p.addReference("ant.PropertyHelper", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42"), true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)} with {@code p}, {@code ref}, {@code reverseLoader}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference, boolean)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRefReverseLoader_whenProject() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42"), true));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_givenAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42")));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>Given empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_givenEmptyString() {
    // Arrange
    Project p = new Project();

    Reference ref = new Reference("42");
    ref.setRefId("");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, ref));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("42", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42")));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>Given {@code Ref}.</li>
   *   <li>When {@link Reference#Reference(String)} with id is {@code 42} RefId is {@code Ref}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_givenRef_whenReferenceWithIdIs42RefIdIsRef() {
    // Arrange
    Project p = new Project();

    Reference ref = new Reference("42");
    ref.setRefId("Ref");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, ref));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code 42} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_givenTarget_whenProjectAddTarget42AndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("42", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42")));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_givenValue() {
    // Arrange
    Project p = new Project();
    p.addReference("ant.PropertyHelper", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42")));
  }

  /**
   * Test {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)} with {@code p}, {@code ref}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getClassLoaderForPath(Project, Reference)}
   */
  @Test
  public void testGetClassLoaderForPathWithPRef_whenProject_thenThrowBuildException() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.getClassLoaderForPath(p, new Reference("42")));
  }

  /**
   * Test {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath_whenNull_thenReturnNotNull() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertNotNull(ClasspathUtils.getUniqueClassLoaderForPath(p, null, false));
    assertEquals(1, p.getBuildListeners().size());
  }

  /**
   * Test {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath_whenPathWithPIsProjectAndPathIsIgnore() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertNotNull(ClasspathUtils.getUniqueClassLoaderForPath(p, new Path(new Project(), "ignore"), true));
    assertEquals(1, p.getBuildListeners().size());
  }

  /**
   * Test {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath_whenPathWithProjectIsNull_thenReturnNotNull() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertNotNull(ClasspathUtils.getUniqueClassLoaderForPath(p, new Path(null), false));
    assertEquals(1, p.getBuildListeners().size());
  }

  /**
   * Test {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#getUniqueClassLoaderForPath(Project, Path, boolean)}
   */
  @Test
  public void testGetUniqueClassLoaderForPath_whenPathWithProjectIsProject_thenReturnNotNull() {
    // Arrange
    Project p = new Project();

    // Act and Assert
    assertNotNull(ClasspathUtils.getUniqueClassLoaderForPath(p, new Path(new Project()), true));
    assertEquals(1, p.getBuildListeners().size());
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> ClasspathUtils.newInstance("Class Name", new AntClassLoader(new AntClassLoader(), true)));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader2() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader3() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Finding class ").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader4() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> ClasspathUtils.newInstance("Class Name", new AntClassLoader(new AntClassLoader(), false)));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader(new AntClassLoader(), true);

    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType2() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType3() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Finding class ").toFile());
    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType4() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader(new AntClassLoader(), false);

    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Finding class ", typeClass);

    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.setProject(project);
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType_givenProject() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.setProject(new Project());
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType_whenAntClassLoader() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader, Class)} with {@code className}, {@code userDefinedLoader}, {@code expectedType}.
   * <ul>
   *   <li>When {@code .class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader, Class)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoaderExpectedType_whenClass() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    Class<Object> expectedType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance(".class", userDefinedLoader, expectedType));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Finding class ", typeClass);

    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.setProject(project);
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader_givenProject() {
    // Arrange
    AntClassLoader userDefinedLoader = new AntClassLoader();
    userDefinedLoader.setProject(new Project());
    userDefinedLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", userDefinedLoader));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader_whenAntClassLoader() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance("Class Name", new AntClassLoader()));
  }

  /**
   * Test {@link ClasspathUtils#newInstance(String, ClassLoader)} with {@code className}, {@code userDefinedLoader}.
   * <ul>
   *   <li>When {@code .class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClasspathUtils#newInstance(String, ClassLoader)}
   */
  @Test
  public void testNewInstanceWithClassNameUserDefinedLoader_whenClass() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ClasspathUtils.newInstance(".class", new AntClassLoader()));
  }

  /**
   * Test {@link ClasspathUtils#getDelegate(ProjectComponent)}.
   * <p>
   * Method under test: {@link ClasspathUtils#getDelegate(ProjectComponent)}
   */
  @Test
  public void testGetDelegate() {
    // Arrange and Act
    Delegate actualDelegate = ClasspathUtils.getDelegate(Path.systemBootClasspath);

    // Assert
    assertNotNull(actualDelegate.getClassLoader());
    assertNull(actualDelegate.getClassLoadId());
    assertNull(actualDelegate.getClasspath());
    assertFalse(actualDelegate.isReverseLoader());
  }
}
