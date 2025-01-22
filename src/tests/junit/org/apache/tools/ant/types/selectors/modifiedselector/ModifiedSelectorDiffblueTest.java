package org.apache.tools.ant.types.selectors.modifiedselector;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Parameter;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector.AlgorithmName;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector.CacheName;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector.ComparatorName;
import org.junit.Test;

public class ModifiedSelectorDiffblueTest {
  /**
   * Test AlgorithmName {@link AlgorithmName#getValues()}.
   * <p>
   * Method under test: {@link AlgorithmName#getValues()}
   */
  @Test
  public void testAlgorithmNameGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"hashvalue", "digest", "checksum", "lastmodified"},
        (new AlgorithmName()).getValues());
  }

  /**
   * Test AlgorithmName new {@link AlgorithmName} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AlgorithmName}
   */
  @Test
  public void testAlgorithmNameNewAlgorithmName() {
    // Arrange and Act
    AlgorithmName actualAlgorithmName = new AlgorithmName();

    // Assert
    assertNull(actualAlgorithmName.getValue());
    assertEquals(-1, actualAlgorithmName.getIndex());
  }

  /**
   * Test CacheName {@link CacheName#getValues()}.
   * <p>
   * Method under test: {@link CacheName#getValues()}
   */
  @Test
  public void testCacheNameGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"propertyfile"}, (new CacheName()).getValues());
  }

  /**
   * Test CacheName new {@link CacheName} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CacheName}
   */
  @Test
  public void testCacheNameNewCacheName() {
    // Arrange and Act
    CacheName actualCacheName = new CacheName();

    // Assert
    assertNull(actualCacheName.getValue());
    assertEquals(-1, actualCacheName.getIndex());
  }

  /**
   * Test ComparatorName {@link ComparatorName#getValues()}.
   * <p>
   * Method under test: {@link ComparatorName#getValues()}
   */
  @Test
  public void testComparatorNameGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"equal", "rule"}, (new ComparatorName()).getValues());
  }

  /**
   * Test ComparatorName new {@link ComparatorName} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ComparatorName}
   */
  @Test
  public void testComparatorNameNewComparatorName() {
    // Arrange and Act
    ComparatorName actualComparatorName = new ComparatorName();

    // Assert
    assertNull(actualComparatorName.getValue());
    assertEquals(-1, actualComparatorName.getIndex());
  }

  /**
   * Test new {@link ModifiedSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ModifiedSelector}
   */
  @Test
  public void testNewModifiedSelector() {
    // Arrange and Act
    ModifiedSelector actualModifiedSelector = new ModifiedSelector();

    // Assert
    Location location = actualModifiedSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualModifiedSelector.getDescription());
    assertNull(actualModifiedSelector.getError());
    assertNull(actualModifiedSelector.getComparator());
    assertNull(actualModifiedSelector.getProject());
    assertNull(actualModifiedSelector.getRefid());
    assertNull(actualModifiedSelector.getAlgorithm());
    assertNull(actualModifiedSelector.getCache());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualModifiedSelector.getModified());
    assertFalse(actualModifiedSelector.isReference());
    assertTrue(actualModifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelector() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code algorithm} and {@code digest}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamAlgorithmAndDigest() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "digest");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code algorithm.} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamAlgorithmAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm.", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code cache.properties} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamCachePropertiesAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code comparator} and {@code equal}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamComparatorAndEqual() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "equal");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code comparator} and {@code rule}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamComparatorAndRule() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "rule");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.verifySettings());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code delayupdate} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamDelayupdateAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("delayupdate", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code seldirs} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamSeldirsAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("seldirs", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code update} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAddParamUpdateAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("update", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) AlgorithmClass is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorAlgorithmClassIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithmClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.verifySettings());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) CacheClass is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorCacheClassIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setCacheClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.verifySettings());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) ComparatorClass is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorComparatorClassIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setComparatorClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.verifySettings());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) Comparator is {@link ComparatorName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenModifiedSelectorComparatorIsComparatorName() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setComparator(new ComparatorName());
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertNull(modifiedSelector.getError());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link Parameter} (default constructor) Name is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenParameterNameIsCacheProperties() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("cache.properties");
    parameter.setType("cache.properties");
    parameter.setValue("42");

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setParameters(parameter);
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link ChecksumAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorAlgorithmChecksumAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "checksum");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof ChecksumAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link HashvalueAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorAlgorithmHashvalueAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "hashvalue");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof HashvalueAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link LastModifiedAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorAlgorithmLastModifiedAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "lastmodified");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof LastModifiedAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Algorithm must be set.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorErrorIsAlgorithmMustBeSet() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithm(new AlgorithmName());
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("Algorithm must be set.", modifiedSelector.getError());
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getAlgorithm());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Cache must be proper configured.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorErrorIsCacheMustBeProperConfigured() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("cache", "propertyfile");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("Cache must be proper configured.", modifiedSelector.getError());
    assertNull(((PropertiesfileCache) cache).getCachefile());
    assertFalse(cache.isValid());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Cache must be set.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorErrorIsCacheMustBeSet() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setCache(new CacheName());

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertEquals("Cache must be set.", modifiedSelector.getError());
    assertNull(modifiedSelector.getCache());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Invalid parameter MD5}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorErrorIsInvalidParameterMd5() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("MD5", "Value");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("Invalid parameter MD5", modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Invalid parameter MD5}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorErrorIsInvalidParameterMd52() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithm(new AlgorithmName());
    modifiedSelector.addParam("MD5", "Value");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("Invalid parameter MD5", modifiedSelector.getError());
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getAlgorithm());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenModifiedSelectorProjectBuildListenersSizeIsOne() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.verifySettings();

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    Vector<BuildListener> buildListeners = modifiedSelector.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertSame(modifiedSelector, buildListeners.get(0));
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given array of {@link Parameter} with {@link Parameter} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenArrayOfParameterWithParameter() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("cache.properties");
    parameter.setType("cache.properties");
    parameter.setValue("42");

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setParameters(parameter);
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code algorithm} and {@code digest}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamAlgorithmAndDigest() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "digest");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code algorithm.} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamAlgorithmAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm.", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code cache.properties} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamCachePropertiesAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code comparator} and {@code equal}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamComparatorAndEqual() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "equal");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code comparator} and {@code rule}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamComparatorAndRule() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "rule");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.configure());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code delayupdate} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamDelayupdateAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("delayupdate", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code seldirs} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamSeldirsAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("seldirs", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code update} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAddParamUpdateAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("update", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) AlgorithmClass is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorAlgorithmClassIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithmClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.configure());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) CacheClass is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorCacheClassIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setCacheClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.configure());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) ComparatorClass is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelectorComparatorClassIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setComparatorClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.configure());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor).</li>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link DigestAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenModifiedSelector_thenModifiedSelectorAlgorithmDigestAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link Parameter} (default constructor) Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenParameterValueIs42() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("cache.properties");
    parameter.setType("cache.properties");
    parameter.setValue("42");

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam(parameter);

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Given {@link Parameter} (default constructor) Value is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_givenParameterValueIsNull() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("cache.properties");
    parameter.setType("cache.properties");
    parameter.setValue(null);

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam(parameter);

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link ChecksumAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorAlgorithmChecksumAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "checksum");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof ChecksumAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link HashvalueAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorAlgorithmHashvalueAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "hashvalue");

    // Act
    modifiedSelector.configure();

    // Assert
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof HashvalueAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorAlgorithmIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithm(new AlgorithmName());
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertNull(modifiedSelector.getAlgorithm());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link LastModifiedAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorAlgorithmLastModifiedAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "lastmodified");

    // Act
    modifiedSelector.configure();

    // Assert
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof LastModifiedAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Cache Cachefile is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorCacheCachefileIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("cache", "propertyfile");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(((PropertiesfileCache) cache).getCachefile());
    assertFalse(cache.isValid());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Comparator is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorComparatorIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setComparator(new ComparatorName());
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertNull(modifiedSelector.getComparator());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorErrorIsCacheProperties() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setError("cache.properties");
    modifiedSelector.addParam("MD5", "Value");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("cache.properties", ((PropertiesfileCache) cache).getCachefile().getName());
    assertEquals("cache.properties", modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Invalid parameter MD5}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorErrorIsInvalidParameterMd5() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("MD5", "Value");
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals("Invalid parameter MD5", modifiedSelector.getError());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#configure()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#configure()}
   */
  @Test
  public void testConfigure_thenModifiedSelectorProjectBuildListenersSizeIsOne() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.configure();

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    Vector<BuildListener> buildListeners = modifiedSelector.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertTrue(modifiedSelector.getDelayUpdate());
    assertSame(modifiedSelector, buildListeners.get(0));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(new AntClassLoader(new AntClassLoader(), true));
    modifiedSelector.addClasspath(Path.systemBootClasspath);
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass2() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(loader);
    modifiedSelector.addClasspath(Path.systemBootClasspath);
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass3() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Finding class ").toFile());

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(loader);
    modifiedSelector.addClasspath(Path.systemBootClasspath);
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass4() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(new AntClassLoader(new AntClassLoader(), false));
    modifiedSelector.addClasspath(Path.systemBootClasspath);
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass_givenAntClassLoaderProjectIsProject_thenThrowBuildException() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.setProject(new Project());
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(loader);
    modifiedSelector.addClasspath(Path.systemBootClasspath);
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addClasspath {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass_givenModifiedSelectorAddClasspathPathWithPIsProjectAndPath() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addClasspath(new Path(new Project(), "Path"));
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addClasspath {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass_givenModifiedSelectorAddClasspathPathWithProjectIsProject() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addClasspath(new Path(new Project()));
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) ClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass_givenModifiedSelectorClassLoaderIsAntClassLoader() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(new AntClassLoader());
    modifiedSelector.addClasspath(Path.systemBootClasspath);
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#loadClass(String, String, Class)}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor).</li>
   *   <li>When {@code Classname}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#loadClass(String, String, Class)}
   */
  @Test
  public void testLoadClass_givenModifiedSelector_whenClassname_thenThrowBuildException() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    Class<Object> type = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.loadClass("Classname", "Msg", type));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("cache.properties", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("update", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile3() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("delayupdate", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile4() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("seldirs", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile5() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm.", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile6() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithmClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile7() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setComparatorClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile8() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setCacheClass("cache.properties");
    modifiedSelector.addParam("cache.properties", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile9() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addParam("cache.properties", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    Vector<BuildListener> buildListeners = modifiedSelector.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertEquals(1, modifiedSelector.getModified());
    assertTrue(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertSame(modifiedSelector, buildListeners.get(0));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile10() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "hashvalue");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof HashvalueAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile11() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "digest");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile12() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "checksum");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof ChecksumAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile13() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm", "lastmodified");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof LastModifiedAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile14() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "equal");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile15() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "rule");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenModifiedSelector() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) Modified is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenModifiedSelectorModifiedIsZero() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setModified(0);
    modifiedSelector.setUpdate(false);
    modifiedSelector.setDelayUpdate(false);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor).</li>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenModifiedSelector_whenEmptyString() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link Parameter} (default constructor) Name is {@code cache.properties}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenParameterNameIsCacheProperties() {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("cache.properties");
    parameter.setType("cache.properties");
    parameter.setValue("42");

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setParameters(parameter);
    modifiedSelector.addParam("cache.properties", "Value");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    modifiedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    assertEquals(0, modifiedSelector.getModified());
    assertFalse(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setSelres(false);
    modifiedSelector.setUpdate(false);
    modifiedSelector.setDelayUpdate(false);
    modifiedSelector.setProject(null);

    FileResource resource = new FileResource();
    resource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    resource.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    modifiedSelector.isSelected(resource);

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource2() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.isSelected(new Resource("modified-", true, 1L));

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource3() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector
        .isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile(), "modified-"));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource4() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector
        .isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), ".."));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource5() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.isSelected(new Resource("modified-", true, Resource.UNKNOWN_SIZE));

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code algorithm.} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorAddParamAlgorithmAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm.", "Value");

    // Act
    modifiedSelector.isSelected(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "modified-"));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code algorithm.} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorAddParamAlgorithmAndValue2() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("algorithm.", "Value");
    modifiedSelector.addParam("java.io.tmpdir", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(new Resource("modified-")));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code cache.properties} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorAddParamCachePropertiesAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("cache.properties", "Value");

    // Act
    modifiedSelector.isSelected(new Resource("modified-"));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code comparator} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorAddParamComparatorAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("comparator", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(new Resource("modified-")));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addParam {@code java.io.tmpdir} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorAddParamJavaIoTmpdirAndValue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addParam("java.io.tmpdir", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(new Resource("modified-")));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) AlgorithmClass is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorAlgorithmClassIsDot() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setAlgorithmClass(".");
    modifiedSelector.addParam("..", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "modified-")));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) CacheClass is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorCacheClassIsDot() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setCacheClass(".");
    modifiedSelector.addParam("..", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "modified-")));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) ComparatorClass is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_givenModifiedSelectorComparatorClassIsDot() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setComparatorClass(".");
    modifiedSelector.addParam("..", "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.isSelected(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "modified-")));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Algorithm {@link DigestAlgorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_thenModifiedSelectorAlgorithmDigestAlgorithm() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.isSelected(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "modified-"));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Comparator is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_thenModifiedSelectorComparatorIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setSelres(false);
    modifiedSelector.setUpdate(false);
    modifiedSelector.setDelayUpdate(false);
    modifiedSelector.setProject(null);

    Resource resource = new Resource();
    resource.setName(null);

    // Act
    boolean actualIsSelectedResult = modifiedSelector.isSelected(resource);

    // Assert
    assertNull(modifiedSelector.getComparator());
    assertNull(modifiedSelector.getAlgorithm());
    assertNull(modifiedSelector.getCache());
    assertFalse(actualIsSelectedResult);
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Modified is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_thenModifiedSelectorModifiedIsOne() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());

    // Act
    modifiedSelector.isSelected(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "modified-"));

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    Vector<BuildListener> buildListeners = modifiedSelector.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertEquals(1, modifiedSelector.getModified());
    assertTrue(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertSame(modifiedSelector, buildListeners.get(0));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_thenModifiedSelectorProjectBuildListenersSizeIsOne() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setSelres(false);
    modifiedSelector.setUpdate(false);
    modifiedSelector.setDelayUpdate(false);
    modifiedSelector.setProject(new Project());

    FileResource resource = new FileResource();
    resource.setBaseDir(null);
    resource.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    modifiedSelector.isSelected(resource);

    // Assert
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    Vector<BuildListener> buildListeners = modifiedSelector.getProject().getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(((PropertiesfileCache) cache).getCachefile().isAbsolute());
    assertSame(modifiedSelector, buildListeners.get(0));
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()} BaseDir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_whenFileResourceBaseDirIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setSelres(false);
    modifiedSelector.setUpdate(false);
    modifiedSelector.setDelayUpdate(false);
    modifiedSelector.setProject(null);

    FileResource resource = new FileResource();
    resource.setBaseDir(null);
    resource.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    modifiedSelector.isSelected(resource);

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@code modified-}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_whenResourceWithNameIsModified() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.isSelected(new Resource("modified-"));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_whenResourceWithNameIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.isSelected(new Resource(null));

    // Assert
    Algorithm algorithm = modifiedSelector.getAlgorithm();
    assertTrue(algorithm instanceof DigestAlgorithm);
    assertTrue(modifiedSelector.getComparator() instanceof EqualComparator);
    Cache cache = modifiedSelector.getCache();
    assertTrue(cache instanceof PropertiesfileCache);
    File cachefile = ((PropertiesfileCache) cache).getCachefile();
    assertEquals("cache.properties", cachefile.getName());
    assertFalse(cachefile.isAbsolute());
    assertFalse(modifiedSelector.getDelayUpdate());
    assertTrue(algorithm.isValid());
    assertTrue(cache.isValid());
  }

  /**
   * Test {@link ModifiedSelector#isSelected(Resource)} with {@code resource}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then {@link ModifiedSelector} (default constructor) DelayUpdate.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#isSelected(Resource)}
   */
  @Test
  public void testIsSelectedWithResource_whenResource_thenModifiedSelectorDelayUpdate() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.isSelected(new Resource());

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#addClasspath(Path)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#addClasspath(Path)}
   */
  @Test
  public void testAddClasspath_thenThrowBuildException() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.addClasspath(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> modifiedSelector.addClasspath(Path.systemBootClasspath));
  }

  /**
   * Test {@link ModifiedSelector#getClassLoader()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) addClasspath {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#getClassLoader()}
   */
  @Test
  public void testGetClassLoader_givenModifiedSelectorAddClasspathPathWithPIsProjectAndPath() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addClasspath(new Path(new Project(), "Path"));

    // Act and Assert
    assertNotNull(modifiedSelector.getClassLoader());
    assertEquals(1, modifiedSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ModifiedSelector#getClassLoader()}.
   * <ul>
   *   <li>Given {@link ModifiedSelector} (default constructor) ClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#getClassLoader()}
   */
  @Test
  public void testGetClassLoader_givenModifiedSelectorClassLoaderIsAntClassLoader() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setClassLoader(new AntClassLoader());
    modifiedSelector.addClasspath(Path.systemBootClasspath);

    // Act and Assert
    assertNotNull(modifiedSelector.getClassLoader());
  }

  /**
   * Test {@link ModifiedSelector#getClassLoader()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#getClassLoader()}
   */
  @Test
  public void testGetClassLoader_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    Path path = new Path(project);

    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addClasspath(path);

    // Act and Assert
    assertNotNull(modifiedSelector.getClassLoader());
    assertEquals(1, modifiedSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ModifiedSelector#getClassLoader()}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#getClassLoader()}
   */
  @Test
  public void testGetClassLoader_thenModifiedSelectorProjectBuildListenersSizeIsOne() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(new Project());
    modifiedSelector.addClasspath(new Path(new Project()));

    // Act and Assert
    assertNotNull(modifiedSelector.getClassLoader());
    assertEquals(1, modifiedSelector.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Given {@code delayupdate}.</li>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_givenDelayupdate_thenModifiedSelectorErrorIsNull() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(null);

    Parameter parameter = new Parameter();
    parameter.setType("Type");
    parameter.setValue("42");
    parameter.setName("delayupdate");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert
    assertNull(modifiedSelector.getError());
    assertFalse(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Given {@code equal}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code equal}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_givenEqual_whenParameterValueIsEqual() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(null);

    Parameter parameter = new Parameter();
    parameter.setType("Type");
    parameter.setValue("equal");
    parameter.setName("comparator");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Given {@code hashvalue}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code hashvalue}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_givenHashvalue_whenParameterValueIsHashvalue() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(null);

    Parameter parameter = new Parameter();
    parameter.setType("Type");
    parameter.setValue("hashvalue");
    parameter.setName("algorithm");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Given {@code propertyfile}.</li>
   *   <li>When {@link Parameter} (default constructor) Value is {@code propertyfile}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_givenPropertyfile_whenParameterValueIsPropertyfile() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(null);

    Parameter parameter = new Parameter();
    parameter.setType("Type");
    parameter.setValue("propertyfile");
    parameter.setName("cache");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Given {@code seldirs}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@code seldirs}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_givenSeldirs_whenParameterNameIsSeldirs() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(null);

    Parameter parameter = new Parameter();
    parameter.setType("Type");
    parameter.setValue("42");
    parameter.setName("seldirs");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Given {@code update}.</li>
   *   <li>When {@link Parameter} (default constructor) Name is {@code update}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_givenUpdate_whenParameterNameIsUpdate() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setProject(null);

    Parameter parameter = new Parameter();
    parameter.setType("Type");
    parameter.setValue("42");
    parameter.setName("update");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert that nothing has changed
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code cache}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_thenModifiedSelectorErrorIsCache() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();
    modifiedSelector.setError("cache");

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert that nothing has changed
    assertEquals("cache", modifiedSelector.getError());
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test {@link ModifiedSelector#useParameter(Parameter)}.
   * <ul>
   *   <li>Then {@link ModifiedSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModifiedSelector#useParameter(Parameter)}
   */
  @Test
  public void testUseParameter_thenModifiedSelectorErrorIsInvalidParameterName() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");

    // Act
    modifiedSelector.useParameter(parameter);

    // Assert
    assertEquals("Invalid parameter Name", modifiedSelector.getError());
    assertTrue(modifiedSelector.getDelayUpdate());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ModifiedSelector#setAlgorithm(AlgorithmName)}
   *   <li>{@link ModifiedSelector#setAlgorithmClass(String)}
   *   <li>{@link ModifiedSelector#setCache(CacheName)}
   *   <li>{@link ModifiedSelector#setCacheClass(String)}
   *   <li>{@link ModifiedSelector#setClassLoader(ClassLoader)}
   *   <li>{@link ModifiedSelector#setComparator(ComparatorName)}
   *   <li>{@link ModifiedSelector#setComparatorClass(String)}
   *   <li>{@link ModifiedSelector#setDelayUpdate(boolean)}
   *   <li>{@link ModifiedSelector#setModified(int)}
   *   <li>{@link ModifiedSelector#setSeldirs(boolean)}
   *   <li>{@link ModifiedSelector#setSelres(boolean)}
   *   <li>{@link ModifiedSelector#setUpdate(boolean)}
   *   <li>{@link ModifiedSelector#buildStarted(BuildEvent)}
   *   <li>{@link ModifiedSelector#messageLogged(BuildEvent)}
   *   <li>{@link ModifiedSelector#targetStarted(BuildEvent)}
   *   <li>{@link ModifiedSelector#taskStarted(BuildEvent)}
   *   <li>{@link ModifiedSelector#toString()}
   *   <li>{@link ModifiedSelector#getAlgorithm()}
   *   <li>{@link ModifiedSelector#getCache()}
   *   <li>{@link ModifiedSelector#getComparator()}
   *   <li>{@link ModifiedSelector#getDelayUpdate()}
   *   <li>{@link ModifiedSelector#getModified()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ModifiedSelector modifiedSelector = new ModifiedSelector();

    // Act
    modifiedSelector.setAlgorithm(new AlgorithmName());
    modifiedSelector.setAlgorithmClass("Classname");
    modifiedSelector.setCache(new CacheName());
    modifiedSelector.setCacheClass("Classname");
    modifiedSelector.setClassLoader(new AntClassLoader());
    modifiedSelector.setComparator(new ComparatorName());
    modifiedSelector.setComparatorClass("Classname");
    modifiedSelector.setDelayUpdate(true);
    modifiedSelector.setModified(1);
    modifiedSelector.setSeldirs(true);
    modifiedSelector.setSelres(true);
    modifiedSelector.setUpdate(true);
    modifiedSelector.buildStarted(new BuildEvent(new Project()));
    modifiedSelector.messageLogged(new BuildEvent(new Project()));
    modifiedSelector.targetStarted(new BuildEvent(new Project()));
    modifiedSelector.taskStarted(new BuildEvent(new Project()));
    String actualToStringResult = modifiedSelector.toString();
    Algorithm actualAlgorithm = modifiedSelector.getAlgorithm();
    Cache actualCache = modifiedSelector.getCache();
    Comparator<? super String> actualComparator = modifiedSelector.getComparator();
    boolean actualDelayUpdate = modifiedSelector.getDelayUpdate();

    // Assert
    assertEquals("{modifiedselector update=true seldirs=true cache=null algorithm=null comparator=null}",
        actualToStringResult);
    assertNull(actualComparator);
    assertNull(actualAlgorithm);
    assertNull(actualCache);
    assertEquals(1, modifiedSelector.getModified());
    assertTrue(actualDelayUpdate);
  }
}
