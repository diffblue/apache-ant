package org.apache.tools.ant.taskdefs.optional.jsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.optional.jsp.JspC.WebAppParameter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JspCDiffblueTest {
  /**
   * Test {@link JspC#createClasspath()}.
   * <ul>
   *   <li>Given {@link JspC} (default constructor).</li>
   *   <li>Then {@link JspC} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspC#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenJspC_thenJspCClasspathDescriptionIsNull() {
    // Arrange
    JspC jspC = new JspC();

    // Act
    Path actualCreateClasspathResult = jspC.createClasspath();

    // Assert
    Path classpath = jspC.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link JspC#createClasspath()}.
   * <ul>
   *   <li>Then {@link JspC} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspC#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenJspCClasspathIsSystemBootClasspath() {
    // Arrange
    JspC jspC = new JspC();
    jspC.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = jspC.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, jspC.getClasspath());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link JspC#setCompiler(String)}
   *   <li>{@link JspC#setDestdir(File)}
   *   <li>{@link JspC#setFailonerror(boolean)}
   *   <li>{@link JspC#setIeplugin(String)}
   *   <li>{@link JspC#setMapped(boolean)}
   *   <li>{@link JspC#setPackage(String)}
   *   <li>{@link JspC#setUriroot(File)}
   *   <li>{@link JspC#setVerbose(int)}
   *   <li>{@link JspC#setWebinc(File)}
   *   <li>{@link JspC#setWebxml(File)}
   *   <li>{@link JspC#getClasspath()}
   *   <li>{@link JspC#getCompileList()}
   *   <li>{@link JspC#getCompilerclasspath()}
   *   <li>{@link JspC#getDestdir()}
   *   <li>{@link JspC#getFailonerror()}
   *   <li>{@link JspC#getIeplugin()}
   *   <li>{@link JspC#getPackage()}
   *   <li>{@link JspC#getSrcDir()}
   *   <li>{@link JspC#getUribase()}
   *   <li>{@link JspC#getUriroot()}
   *   <li>{@link JspC#getVerbose()}
   *   <li>{@link JspC#getWebApp()}
   *   <li>{@link JspC#getWebinc()}
   *   <li>{@link JspC#getWebxml()}
   *   <li>{@link JspC#isMapped()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    JspC jspC = new JspC();

    // Act
    jspC.setCompiler("Compiler");
    File destDir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    jspC.setDestdir(destDir);
    jspC.setFailonerror(true);
    jspC.setIeplugin("Iepluginid");
    jspC.setMapped(true);
    jspC.setPackage("Pkg");
    File uriroot = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    jspC.setUriroot(uriroot);
    jspC.setVerbose(1);
    File webinc = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    jspC.setWebinc(webinc);
    File webxml = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    jspC.setWebxml(webxml);
    Path actualClasspath = jspC.getClasspath();
    Vector<String> actualCompileList = jspC.getCompileList();
    Path actualCompilerclasspath = jspC.getCompilerclasspath();
    File actualDestdir = jspC.getDestdir();
    boolean actualFailonerror = jspC.getFailonerror();
    String actualIeplugin = jspC.getIeplugin();
    String actualPackage = jspC.getPackage();
    Path actualSrcDir = jspC.getSrcDir();
    File actualUribase = jspC.getUribase();
    File actualUriroot = jspC.getUriroot();
    int actualVerbose = jspC.getVerbose();
    WebAppParameter actualWebApp = jspC.getWebApp();
    File actualWebinc = jspC.getWebinc();
    File actualWebxml = jspC.getWebxml();
    boolean actualIsMappedResult = jspC.isMapped();

    // Assert
    assertEquals("Iepluginid", actualIeplugin);
    assertEquals("Pkg", actualPackage);
    assertNull(actualWebApp);
    assertNull(actualClasspath);
    assertNull(actualCompilerclasspath);
    assertNull(actualSrcDir);
    assertEquals(1, actualVerbose);
    assertTrue(actualCompileList.isEmpty());
    assertTrue(actualFailonerror);
    assertTrue(actualIsMappedResult);
    assertSame(destDir, actualDestdir);
    assertSame(uriroot, actualUribase);
    assertSame(uriroot, actualUriroot);
    assertSame(webinc, actualWebinc);
    assertSame(webxml, actualWebxml);
  }

  /**
   * Test {@link JspC#createCompilerclasspath()}.
   * <ul>
   *   <li>Given {@link JspC} (default constructor).</li>
   *   <li>Then {@link JspC} (default constructor) Compilerclasspath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspC#createCompilerclasspath()}
   */
  @Test
  public void testCreateCompilerclasspath_givenJspC_thenJspCCompilerclasspathDescriptionIsNull() {
    // Arrange
    JspC jspC = new JspC();

    // Act
    Path actualCreateCompilerclasspathResult = jspC.createCompilerclasspath();

    // Assert
    Path compilerclasspath = jspC.getCompilerclasspath();
    assertNull(compilerclasspath.getDescription());
    assertNull(actualCreateCompilerclasspathResult.getProject());
    assertNull(compilerclasspath.getProject());
    assertNull(compilerclasspath.getRefid());
    assertEquals(0, compilerclasspath.size());
    assertFalse(compilerclasspath.isReference());
    assertTrue(compilerclasspath.isEmpty());
  }

  /**
   * Test {@link JspC#createCompilerclasspath()}.
   * <ul>
   *   <li>Then {@link JspC} (default constructor) Compilerclasspath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspC#createCompilerclasspath()}
   */
  @Test
  public void testCreateCompilerclasspath_thenJspCCompilerclasspathIsSystemBootClasspath() {
    // Arrange
    JspC jspC = new JspC();
    jspC.setCompilerclasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedCompilerclasspath = jspC.createCompilerclasspath().systemBootClasspath;
    assertSame(expectedCompilerclasspath, jspC.getCompilerclasspath());
  }

  /**
   * Test {@link JspC#addWebApp(WebAppParameter)}.
   * <ul>
   *   <li>Given {@link JspC} (default constructor).</li>
   *   <li>Then {@link JspC} (default constructor) WebApp is {@link WebAppParameter} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JspC#addWebApp(WebAppParameter)}
   */
  @Test
  public void testAddWebApp_givenJspC_thenJspCWebAppIsWebAppParameter() throws BuildException {
    // Arrange
    JspC jspC = new JspC();

    WebAppParameter webappParam = new WebAppParameter();
    webappParam.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    jspC.addWebApp(webappParam);

    // Assert
    assertSame(webappParam, jspC.getWebApp());
  }

  /**
   * Test {@link JspC#addWebApp(WebAppParameter)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspC#addWebApp(WebAppParameter)}
   */
  @Test
  public void testAddWebApp_thenThrowBuildException() throws BuildException {
    // Arrange
    WebAppParameter webappParam = new WebAppParameter();
    webappParam.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    JspC jspC = new JspC();
    jspC.addWebApp(webappParam);

    WebAppParameter webappParam2 = new WebAppParameter();
    webappParam2.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> jspC.addWebApp(webappParam2));
  }

  /**
   * Test new {@link JspC} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JspC}
   */
  @Test
  public void testNewJspC() {
    // Arrange and Act
    JspC actualJspC = new JspC();

    // Assert
    assertNull(actualJspC.getDestdir());
    assertNull(actualJspC.getUribase());
    assertNull(actualJspC.getUriroot());
    assertNull(actualJspC.getWebinc());
    assertNull(actualJspC.getWebxml());
    assertNull(actualJspC.getDescription());
    assertNull(actualJspC.getTaskName());
    assertNull(actualJspC.getTaskType());
    assertNull(actualJspC.getIeplugin());
    assertNull(actualJspC.getPackage());
    assertNull(actualJspC.getProject());
    assertNull(actualJspC.getOwningTarget());
    assertNull(actualJspC.getWebApp());
    assertNull(actualJspC.getClasspath());
    assertNull(actualJspC.getCompilerclasspath());
    assertNull(actualJspC.getSrcDir());
    assertEquals(0, actualJspC.getVerbose());
    assertFalse(actualJspC.hasSelectors());
    assertFalse(actualJspC.isMapped());
    assertTrue(actualJspC.getCompileList().isEmpty());
    assertTrue(actualJspC.javaFiles.isEmpty());
    assertTrue(actualJspC.getFailonerror());
  }

  /**
   * Test WebAppParameter getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link WebAppParameter}
   *   <li>{@link WebAppParameter#setBaseDir(File)}
   *   <li>{@link WebAppParameter#getDirectory()}
   * </ul>
   */
  @Test
  public void testWebAppParameterGettersAndSetters() {
    // Arrange and Act
    WebAppParameter actualWebAppParameter = new WebAppParameter();
    File directory = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualWebAppParameter.setBaseDir(directory);

    // Assert
    assertSame(directory, actualWebAppParameter.getDirectory());
  }
}
